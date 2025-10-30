from __future__ import annotations
from typing import Iterable

from .semtype_core import (
    SemType,
    AtomicType,
    FuncType,
    OptionalType,
    flatten_options,
    normalize_type_params,
    unroll_exponent_once,
    same_connector,
    same_synfeats,
)


def _eq_atomic(a: AtomicType, b: AtomicType) -> bool:
    return (a.name == b.name) and same_synfeats(a.synfeats, b.synfeats)


def _eq_optional(a: OptionalType, b: OptionalType) -> bool:
    if len(a.options) != len(b.options):
        return False
    # Compare option sets up to semantic equality
    unmatched = list(b.options)
    for x in a.options:
        for j, y in enumerate(unmatched):
            if semtype_equal(x, y):
                unmatched.pop(j)
                break
        else:
            return False
    return True


def _eq_func(a: FuncType, b: FuncType) -> bool:
    # Normalize before compare:
    aN = normalize_type_params(unroll_exponent_once(a))
    bN = normalize_type_params(unroll_exponent_once(b))
    return (
        same_connector(aN.conn, bN.conn)
        and same_synfeats(aN.synfeats, bN.synfeats)
        and semtype_equal(aN.domain, bN.domain)
        and semtype_equal(aN.range, bN.range)
        and aN.exponent == bN.exponent
    )


def semtype_equal(t1: SemType, t2: SemType) -> bool:
    """
    Lisp: semtype-equal?
    Deep semantic equality (after basic normalization).
    """
    # Flatten optional groups (choices) to canonical structure first:
    t1F = flatten_options(t1)
    t2F = flatten_options(t2)

    if isinstance(t1F, AtomicType) and isinstance(t2F, AtomicType):
        return _eq_atomic(t1F, t2F)
    if isinstance(t1F, OptionalType) and isinstance(t2F, OptionalType):
        return _eq_optional(t1F, t2F)
    if isinstance(t1F, FuncType) and isinstance(t2F, FuncType):
        return _eq_func(t1F, t2F)
    return False


def _match_optional(pattern: OptionalType, inst: SemType) -> bool:
    # Instance must match one of the options
    return any(semtype_match(opt, inst) for opt in pattern.options)


def _match_atomic(pattern: AtomicType, inst: SemType) -> bool:
    if not isinstance(inst, AtomicType):
        return False
    # Atom match is exact symbol (and features subset if needed)
    # Here we require equality of name; features are treated permissively by default.
    # Adjust if you need stricter semantics.
    if pattern.name != inst.name:
        return False
    # pattern features must be subset of instance features (Lisp sometimes
    # has negated syntax flags; plug here if needed)
    return pattern.synfeats.issubset(inst.synfeats)


def _match_func(pattern: FuncType, inst: SemType) -> bool:
    if not isinstance(inst, FuncType):
        return False
    # Connectors must agree (>>, =>)
    if not same_connector(pattern.conn, inst.conn):
        return False
    # Exponent / valency must be equal (after one unroll to align)
    pN = normalize_type_params(unroll_exponent_once(pattern))
    iN = normalize_type_params(unroll_exponent_once(inst))
    if pN.exponent != iN.exponent:
        return False
    if not _synfeat_match(pN.synfeats, iN.synfeats):
        return False
    return semtype_match(pN.domain, iN.domain) and semtype_match(pN.range, iN.range)


def _synfeat_match(patts: set[str], insts: set[str]) -> bool:
    # Basic: pattern synfeats must be subset of instance synfeats
    return patts.issubset(insts)


def semtype_match(pattern: SemType, instance: SemType) -> bool:
    """
    Lisp: semtype-match?
    Optional groups in pattern are treated as choices.
    """
    patternF = flatten_options(pattern)
    instanceF = flatten_options(instance)

    if isinstance(patternF, OptionalType):
        return _match_optional(patternF, instanceF)
    if isinstance(patternF, AtomicType):
        return _match_atomic(patternF, instanceF)
    if isinstance(patternF, FuncType):
        return _match_func(patternF, instanceF)
    return False
