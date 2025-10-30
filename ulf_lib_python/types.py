from __future__ import annotations
from dataclasses import replace
from typing import Optional, Literal

from .semtype_core import SemType, AtomicType, FuncType, OptionalType
from .semtype_parse import extended_str2semtype
from .semtype_equal import semtype_match


def _to_string(t: SemType) -> str:
    """Very small pretty-printer sufficient for the tests we’re adding now."""
    if isinstance(t, AtomicType):
        return t.name
    if isinstance(t, FuncType):
        return f"({_to_string(t.domain)}{t.conn}{_to_string(t.range)})"
    if isinstance(t, OptionalType):
        # keep original order; caller may sort if needed elsewhere
        return "{" + "|".join(_to_string(o) for o in t.options) + "}"
    return "<?>"

def _apply_operator(opr: SemType, arg: SemType) -> Optional[SemType]:
    """
    Core EL application (minimal):
      - Optional operator/argument: try branches and merge successful results.
      - Functional operator: if domain matches arg, return (copy of) range.
      - Otherwise: fail (None).
    We intentionally ignore suffix/synfeat propagation for now.
    """
    # Optional operator
    if isinstance(opr, OptionalType):
        outs = [ _apply_operator(o, arg) for o in opr.options ]
        outs = [o for o in outs if o is not None]
        if not outs:
            return None
        if len(outs) == 1:
            return outs[0]
        return OptionalType(tuple(outs))

    # Optional argument
    if isinstance(arg, OptionalType):
        outs = [ _apply_operator(opr, a) for a in arg.options ]
        outs = [o for o in outs if o is not None]
        if not outs:
            return None
        if len(outs) == 1:
            return outs[0]
        return OptionalType(tuple(outs))

    # Functional operator
    if isinstance(opr, FuncType):
        if semtype_match(opr.domain, arg):
            # return a copy of the range (no feature/suffix merge yet)
            if isinstance(opr.range, FuncType):
                return replace(opr.range)  # shallow is enough for tests here
            return opr.range
    return None


class SemTypeSystem:
    """
    Unified façade for composing semtypes and returning strings,
    with support for two variants:
      - variant=None or 'extended' : normal EL with optional branching
      - variant='left-right'       : also try flipping (arg, opr) first
    """
    def compose_types_to_string(
        self,
        type_str1: str,
        type_str2: str,
        *,
        variant: Optional[Literal["extended", "left-right"]] = None,
    ) -> str | None:
        t1 = extended_str2semtype(type_str1)
        t2 = extended_str2semtype(type_str2)

        if variant == "left-right":
            # Try right-application first (subject + VP): t2(t1)
            out = _apply_operator(t2, t1)
            if out is not None:
                return _to_string(out)
            # Fallback to standard order
            out = _apply_operator(t1, t2)
            return _to_string(out) if out is not None else None

        # Default / extended variant: standard order, then flipped
        out = _apply_operator(t1, t2)
        return _to_string(out) if out is not None else None
