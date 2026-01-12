"""Common error classes for MultiGen backends.

This module provides shared exception classes used across all backend implementations
for consistent error handling and reporting.

For backward compatibility, this re-exports the enhanced error classes from multigen.errors.
New code should import from multigen.errors directly.
"""

# Re-export enhanced error classes for backward compatibility
from ..errors import (
    ErrorCode,
    MultiGenError,
    SourceLocation,
    TypeMappingError,
    UnsupportedFeatureError,
    create_unsupported_feature_error,
    suggest_fix,
)

__all__ = [
    "UnsupportedFeatureError",
    "TypeMappingError",
    "MultiGenError",
    "SourceLocation",
    "ErrorCode",
    "create_unsupported_feature_error",
    "suggest_fix",
]
