from __future__ import annotations
import re
from typing import Tuple

# These are pragmatic helpers, not a full clone of the Lisp suffix logic.
# They’re sufficient for parsing the simple forms the tests rely on.

ATOM_SUFFIX_RE = re.compile(r"^(.*?)(?:\.([A-Za-z][A-Za-z\-]*))$")  # e.g., man.n -> (man, n)


def has_suffix(token: str) -> bool:
    return ATOM_SUFFIX_RE.match(token) is not None


def split_by_suffix(token: str) -> Tuple[str, str | None]:
    """
    Return (stem, suffix?) for strings like "man.n" or "be.v".
    If no suffix is present, returns (token, None).
    """
    m = ATOM_SUFFIX_RE.match(token)
    if not m:
        return token, None
    return m.group(1), m.group(2)


def strip_suffix(token: str) -> str:
    return split_by_suffix(token)[0]


def add_suffix(stem: str, suffix: str) -> str:
    if not suffix:
        return stem
    if suffix.startswith("."):
        return stem + suffix
    return f"{stem}.{suffix}"


def suffix_for_type(typestr: str) -> str | None:
    """
    Heuristic: pick a nominal suffix for a type string (e.g., a simple atom).
    This is a thin compatibility bridge used in some pipelines; return None
    when not meaningful.
    """
    # If it's a bare atom (like "D" or "S"), we don't force any suffix.
    if re.fullmatch(r"[A-Za-z0-9]+", typestr or ""):
        return None
    return None
