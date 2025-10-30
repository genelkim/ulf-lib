import pytest
from ulf_lib_python.composition import (
    extended_compose_type_string,
    left_right_compose_type_string,
)

class TestComposeTypeStringWrappers:
    def test_extended_compose_success(self):
        assert extended_compose_type_string("(D=>S)", "D") == "S"

    def test_extended_compose_fail_wrong_order(self):
        assert extended_compose_type_string("D", "(D=>S)") is None

    def test_left_right_compose_rightward(self):
        assert left_right_compose_type_string("(D=>S)", "D") == "S"

    def test_left_right_compose_leftward_subject_vp(self):
        assert left_right_compose_type_string("D", "(D=>S)") == "S"

    def test_extended_optional_operator_pick_branch(self):
        assert extended_compose_type_string("{(D=>S)|(S=>2)}", "D") == "S"

    def test_extended_optional_argument_pick_branch(self):
        assert extended_compose_type_string("(D=>S)", "{D|S}") == "S"