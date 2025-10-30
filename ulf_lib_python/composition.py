from __future__ import annotations
from typing import Literal, Optional, Tuple

from .semtype_parse import extended_str2semtype
from .semtype_equal import semtype_equal, semtype_match
from .types import SemTypeSystem


def semtype_equal_str(str1: str, str2: str) -> bool:
    """
    Lisp: semtype-equal?-str
    Convert two semtype strings into internal structures and test equality
    under the same normalization rules.
    """
    t1 = extended_str2semtype(str1)
    t2 = extended_str2semtype(str2)
    return semtype_equal(t1, t2)


def semtype_match_str(pattern: str, instance: str) -> bool:
    """
    Lisp (used in tests): semtype-match?-str
    Does 'instance' match the 'pattern'?
      - pattern and instance are semtype-string representations.
    """
    p = extended_str2semtype(pattern)
    i = extended_str2semtype(instance)
    return semtype_match(p, i)


def string_from_compose_types(
    ulf1: object,
    ulf2: object,
    *,
    ext: Optional[Literal["extended", "left-right"]] = None,
) -> str | None:
    """
    Lisp helper:
      Given two atomic ULF expressions, compose their types and return a string.
    Matches Lisp's string-from-compose-types: compute ulf-type-string? for each
    and dispatch to the right compose-*-type-string! variant.
    """
    from .ulf_type import ulf_type_string

    type1 = ulf_type_string(ulf1)
    type2 = ulf_type_string(ulf2)

    # Choose the composition variant like the Lisp CASE form
    if ext == "extended":
        return extended_compose_type_string(type1, type2) if type1 and type2 else None
    if ext == "left-right":
        return left_right_compose_type_string(type1, type2) if type1 and type2 else None
    # default (compose-type-string!)
    from .types import SemTypeSystem
    sts = SemTypeSystem()
    return sts.compose_types_to_string(type1, type2) if type1 and type2 else None



def extended_compose_type_string(int1: str, int2: str) -> str | None:
    """
    Lisp: extended-compose-type-string!
    Compose two *type strings* using the 'extended' variant.
    Returns the composed type as a string, or None if unmatchable.
    """
    from .types import SemTypeSystem
    sts = SemTypeSystem()
    return sts.compose_types_to_string(int1, int2, variant="extended")


def left_right_compose_type_string(int1: str, int2: str) -> str | None:
    """
    Lisp: left-right-compose-type-string!
    Compose two *type strings* using the 'left-right' variant.
    Returns the composed type as a string, or None if unmatchable.
    """
    from .types import SemTypeSystem
    sts = SemTypeSystem()
    return sts.compose_types_to_string(int1, int2, variant="left-right")
