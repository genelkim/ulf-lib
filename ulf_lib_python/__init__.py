# Expose a tidy public surface
from .composition import (
    semtype_equal_str,        # test helper: compare two type strings
    semtype_match_str,        # test helper: pattern vs instance (string)
    string_from_compose_types # test helper: compose two ULF atoms -> type str
)

from .types import (
    SemTypeSystem,  # unified façade you used in tests
)

# Frequently used helpers
from .semtype_parse import extended_str2semtype, str2semtype
from .semtype_equal import semtype_equal, semtype_match
from .suffix import (
    has_suffix, split_by_suffix, strip_suffix, add_suffix, suffix_for_type
)
