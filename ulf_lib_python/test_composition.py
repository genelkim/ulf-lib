# ulf_lib_python/test_semtype_equal_str.py
import pytest

from ulf_lib_python.composition import semtype_equal_str, string_from_compose_types

class TestSemTypeEqualStr:
    # 1) Optional sets: order-insensitive equality
    def test_optionals_order_insensitive(self):
        assert semtype_equal_str("{D|S|2}", "{S|D|2}")
        assert semtype_equal_str("{(D=>S)|(S=>2)}", "{(S=>2)|(D=>S)}")

    # 2) Nested optionals with functions: still order-insensitive
    def test_nested_optionals_of_functions(self):
        a = "{(D=>S)|(S=>2)|(D=>2)}"
        b = "{(S=>2)|(D=>2)|(D=>S)}"
        assert semtype_equal_str(a, b)

    # 3) Simple atoms: equal only when identical (features match exactly)
    def test_atomic_with_features(self):
        assert semtype_equal_str("D%T", "D%T")
        assert not semtype_equal_str("D%T", "D%X")
        assert not semtype_equal_str("D", "S")

    # 4) Function types: connector, exponent, and synfeats must match
    def test_function_structural_equality(self):
        assert semtype_equal_str("(D=>S)", "(D=>S)")
        assert not semtype_equal_str("(D=>S)", "(D>>S)")   # different connector
        assert semtype_equal_str("(D=> (S=>2))", "(D=> (S=>2))")

    # 5) Optionals containing atoms vs same atoms re-ordered
    def test_optionals_atoms_varied_order(self):
        assert semtype_equal_str("{A|B|C}", "{C|B|A}")
        assert semtype_equal_str("{A|B}", "{B|A}")
        assert not semtype_equal_str("{A|B}", "{A|B|C}")

    # 6) Optionals containing mixed atoms and functions
    def test_mixed_atoms_and_functions(self):
        x = "{D|(D=>S)}"
        y = "{(D=>S)|D}"
        assert semtype_equal_str(x, y)

    # 7) Feature sets equality on functions (order-insensitive within % block)
    def test_function_features(self):
        assert semtype_equal_str("(D=>S)%T", "(D=>S)%T")
        assert not semtype_equal_str("(D=>S)%T", "(D=>S)%LEX")


class TestStringFromComposeTypesBasic:
    def test_det_plus_noun(self):
        # (the.d man.n) ⇒ D
        assert string_from_compose_types("the.d", "man.n") == "D"

    def test_invalid_same_category(self):
        # (the.d the.d) ⇒ nil
        assert string_from_compose_types("the.d", "the.d") is None