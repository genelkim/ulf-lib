from __future__ import annotations
import re
from typing import List, Tuple

from .semtype_core import (
    SemType, AtomicType, FuncType, OptionalType,
    new_atomic, new_func, new_optional,
)

# Very small grammar to cover tests; grow as needed.
# Supported:
#   - Atoms: D, S, 2, etc. Optional suffix blocks like "_V", "%LEX", "%T" etc.
#   - Function: "(A=>B)" or "(A>>(B))" (allow spaces)
#   - Optional groups: "{A|B|C}"
#   - Simple exponents: "(S=>2)" where '2' is atomic (we keep as AtomicType("2"))
#
# NOTE: Full ULF grammar is large; this parser is intentionally conservative
# but structured so you can extend quickly.


_SP = r"\s*"
NAME = r"[A-Za-z0-9]+"
ATOM_RE = re.compile(rf"{_SP}({NAME})(?:_([A-Za-z]+))?(?:%([A-Za-z,!\-]+))?{_SP}$")
# e.g., D_V%LEX

def extended_str2semtype(s: str) -> SemType:
    """
    Lisp: extended-str2semtype
    Parses a semtype string into an internal SemType node.
    """
    s = s.strip()
    if s.startswith("{") and s.endswith("}"):
        return _parse_optional(s[1:-1])
    if s.startswith("(") and s.endswith(")"):
        return _parse_func(s[1:-1])
    return _parse_atom(s)


def str2semtype(s: str) -> SemType:
    """
    Base parser (same behavior here, but split for parity with Lisp naming).
    """
    return extended_str2semtype(s)


def _parse_optional(body: str) -> OptionalType:
    # Split by top-level '|' respecting nested () and {}
    parts = _split_top(body, sep="|")
    opts = [extended_str2semtype(p) for p in parts]
    return new_optional(opts)


def _parse_func(body: str) -> FuncType:
    # Split only the top-level connector => or >>
    # Preference: '=>' if present, else '>>'
    left_right = _split_top_first(body, "=>")
    conn = "=>"
    if left_right is None:
        left_right = _split_top_first(body, ">>")
        conn = ">>"
    if left_right is None:
        # Treat as atom fallback if stray parens
        return _func_fallback(body)

    left, right = left_right
    dom = extended_str2semtype(left)
    ran = extended_str2semtype(right)

    # Extract any trailing "_X" and "%FEATS" after the right side.
    # e.g. "(S=>2)_V%T"
    suffix_name, feats = _grab_suffix_and_feats_after(body)
    synfeats = set()
    if feats:
        synfeats = set([x for x in feats.split(",") if x])

    return new_func(dom, ran, exponent=1, conn=conn, synfeats=synfeats)


def _func_fallback(body: str) -> FuncType:
    # Attempt a last-ditch split around '=>'
    if "=>" in body or ">>" in body:
        conn = "=>" if "=>" in body else ">>"
        left, right = body.split(conn, 1)
        return new_func(
            extended_str2semtype(left),
            extended_str2semtype(right),
            exponent=1,
            conn=conn,
            synfeats=set(),
        )
    # Otherwise treat as atom inside parens
    return new_func(new_atomic("⊥"), new_atomic("⊥"), 1, "=>", set())  # unlikely path


def _parse_atom(s: str) -> AtomicType:
    m = ATOM_RE.match(s)
    if not m:
        # Fallback: raw name without suffixes
        name = s.strip()
        return new_atomic(name)
    name, suffix_block, feat_block = m.groups()
    # We keep suffix_block unused here (can be threaded into AtomicType if needed)
    synfeats = set()
    if feat_block:
        for bit in feat_block.split(","):
            bit = bit.strip()
            if bit:
                synfeats.add(bit)
    return new_atomic(name, synfeats=synfeats)


def _grab_suffix_and_feats_after(s: str) -> Tuple[str | None, str | None]:
    """
    For a string like 'A=>B)_V%T' (minus leading '(')
    Return (suffix_name, features_string) if present.
    Current code does not synthesize suffix_name; we only care about %feature set in core.
    """
    tail = s.strip()
    # Find pattern ")_SUFFIX%FEATS" at end.
    # Minimal: return only feats to feed synfeats.
    m = re.search(r"\)(?:_([A-Za-z]+))?(?:%([A-Za-z,!\-]+))?\s*$", tail)
    if not m:
        return None, None
    return m.group(1), m.group(2)


def _split_top(s: str, sep: str) -> List[str]:
    """Split by sep at top level (not inside () or {})."""
    res = []
    buf = []
    depth_par = 0
    depth_brace = 0
    i = 0
    while i < len(s):
        ch = s[i]
        if ch == "(":
            depth_par += 1
        elif ch == ")":
            depth_par -= 1
        elif ch == "{":
            depth_brace += 1
        elif ch == "}":
            depth_brace -= 1
        if depth_par == 0 and depth_brace == 0 and s.startswith(sep, i):
            res.append("".join(buf).strip())
            buf = []
            i += len(sep)
            continue
        buf.append(ch)
        i += 1
    if buf:
        res.append("".join(buf).strip())
    return res


def _split_top_first(s: str, sep: str) -> Tuple[str, str] | None:
    res = _split_top(s, sep)
    if len(res) == 2:
        return res[0], res[1]
    return None
