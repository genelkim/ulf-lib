from __future__ import annotations
from typing import Any, Dict

from .semtype_parse import extended_str2semtype
from .types import _to_string  # small pretty-printer already in types.py


# --- Minimal lexicon (only what we need now, faithful to Lisp conventions) ---

# Generic monadic predicate types by POS:
#   N : (D => (S => 2))_N %LEX
#   V : (D => (S => 2))_V %LEX
#   A : (D => (S => 2))_A %LEX
NOUN_TYPE_STR = "(D=>(S=>2))_N%LEX"
VERB_TYPE_STR = "(D=>(S=>2))_V%LEX"
ADJ_TYPE_STR  = "(D=>(S=>2))_A%LEX"

# Term type:
TERM_TYPE_STR = "D"

# Determiners: { ((D=>(S=>2))_N => D)%LEX | ((D=>(S=>2))_P => D)%LEX }
# (matches what the Lisp tests assert for a.d)
DET_TYPE_STR = "{((D=>(S=>2))_N=>D)%LEX|((D=>(S=>2))_P=>D)%LEX}"

# Sentential punctuation (used later):
BANG_QMARK_TYPE_STR = "((S=>2)_V>>(S=>2))%LEX"


def _ulf_atom_to_typestr(atom: str) -> str | None:
    """
    Translate a single atomic ULF symbol (as a Python string) to a semtype string.
    This mirrors just the pieces used by the tests we're porting first.
    """
    # Common suffix-based mapping
    if atom.endswith(".n"):
        return NOUN_TYPE_STR
    if atom.endswith(".v"):
        return VERB_TYPE_STR
    if atom.endswith(".a"):
        return ADJ_TYPE_STR
    if atom.endswith(".pro"):
        return TERM_TYPE_STR
    if atom.endswith(".d"):
        return DET_TYPE_STR

    # Exact atoms (as they appear in the Lisp tests)
    if atom == "!":
        return BANG_QMARK_TYPE_STR
    if atom == "?":
        return BANG_QMARK_TYPE_STR

    # If unknown/unused in current test scope, return None (Lisp would treat as nil)
    return None


def ulf_type(ulf: Any, *, lambda_vars: list[str] | None = None):
    """
    Python translation of Lisp: ulf-type?
    Minimal version for now:
      - If ULF is an atom string, return its semtype (or None if unknown)
      - If ULF is a one-element list/tuple, recurse into that element
      - (lambda ...) and complex composition cases will be extended later
    """
    if lambda_vars is None:
        lambda_vars = []

    # Atomic ULF (we use Python str to represent an atom/symbol)
    if isinstance(ulf, str):
        # Special cases mirroring Lisp behavior could go here (e.g., '\"', *qt, etc.)
        # For now we cover the atoms the current tests require.
        typestr = _ulf_atom_to_typestr(ulf)
        return extended_str2semtype(typestr) if typestr is not None else None

    # Single-element non-atomic (e.g., ['x']) -> ulf-type? of inner
    if isinstance(ulf, (list, tuple)) and len(ulf) == 1:
        return ulf_type(ulf[0], lambda_vars=lambda_vars)

    # For everything else (compound forms) we’ll implement when tests reach them
    return None


def ulf_type_string(ulf: Any) -> str | None:
    """
    Python translation of Lisp: ulf-type-string?
    Returns the pretty-printed semtype string, or None if unknown.
    """
    t = ulf_type(ulf)
    return _to_string(t) if t is not None else None
