from __future__ import annotations
from dataclasses import dataclass, replace
from typing import Set, List, Literal, Union

Connector = Literal["=>", ">>"]


@dataclass(frozen=True)
class SemType:
    pass


@dataclass(frozen=True)
class AtomicType(SemType):
    name: str
    synfeats: Set[str]


@dataclass(frozen=True)
class FuncType(SemType):
    domain: SemType
    range: SemType
    exponent: int            # valency / arity marker if used
    conn: Connector          # '=>' or '>>'
    synfeats: Set[str]


@dataclass(frozen=True)
class OptionalType(SemType):
    options: tuple[SemType, ...]


def new_atomic(name: str, synfeats: Set[str] | None = None) -> AtomicType:
    return AtomicType(name=name, synfeats=set() if synfeats is None else set(synfeats))


def new_func(
    domain: SemType,
    range: SemType,
    exponent: int,
    conn: Connector,
    synfeats: Set[str] | None = None,
) -> FuncType:
    return FuncType(
        domain=domain,
        range=range,
        exponent=exponent,
        conn=conn,
        synfeats=set() if synfeats is None else set(synfeats),
    )


def new_optional(opts: List[SemType]) -> OptionalType:
    return OptionalType(options=tuple(opts))


def flatten_options(t: SemType) -> SemType:
    """
    Bring nested optionals to a single OptionalType node where possible.
    (Lightweight: we only collapse OptionalType[OptionalType[...]] at top levels.)
    """
    if isinstance(t, OptionalType):
        flat: list[SemType] = []
        for o in t.options:
            oF = flatten_options(o)
            if isinstance(oF, OptionalType):
                flat.extend(list(oF.options))
            else:
                flat.append(oF)
        return OptionalType(tuple(flat))
    if isinstance(t, FuncType):
        return replace(t, domain=flatten_options(t.domain), range=flatten_options(t.range))
    return t


def normalize_type_params(t: SemType) -> SemType:
    """
    Placeholder for normalizing parameter lists / order, etc.
    For now pass through; hook your rules here if needed.
    """
    return t


def unroll_exponent_once(t: SemType) -> SemType:
    """
    In Lisp, exponent markers can encode valency; some routines unroll/align once.
    Here we leave exponent as-is and just return the node; customize if needed.
    """
    return t


def same_connector(a: str, b: str) -> bool:
    return a == b


def same_synfeats(a: Set[str], b: Set[str]) -> bool:
    # Order-insensitive equality
    return a == b
