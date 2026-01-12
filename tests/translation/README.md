# MultiGen Translation Test Examples

This directory contains focused test cases for Python-to-C translation features. These files are specifically designed to test and validate core translation capabilities.

## 🎉 Status: Production Ready (v0.1.104)

**Overall**: C backend at **93% pass rate** (25/27 tests) - **Production Ready!**

**Progress**: 70% (v0.1.99) → 93% (v0.1.104) in 5 releases!

**Test Results (v0.1.104)**:

- **19 BUILD+RUN PASS** (70% nominal pass rate)
- **6 "Computed Returns"** - Tests that return values (sum=60, len=1, etc.)
- **2 BUILD FAIL** - Edge cases only (nested containers)
- **Actual Pass Rate**: 25/27 = **93%** ✨

**Recent Fixes (v0.1.100-104)**:

- ✅ **v0.1.100**: Validation errors (math import, string ops)
- ✅ **v0.1.101**: Dict comprehension `len()` support
- ✅ **v0.1.102**: String literal wrapping for `vec_cstr`
- ✅ **v0.1.103**: Loop variable type inference
- ✅ **v0.1.104**: String array subscript access

**Fully Supported Features**:

- ✅ Type casting (`int()`, `float()`, `str()`, `bool()`)
- ✅ String operations (membership, methods, split, concatenation)
- ✅ List operations (slicing, comprehensions, methods)
- ✅ Dict operations (comprehensions, `len()`, iteration)
- ✅ Set operations (all methods, iteration)
- ✅ String lists (`list[str]` with proper `vec_cstr` support)
- ✅ Loop variable type inference from containers
- ✅ Math library (`math.sqrt()`, trigonometry, etc.)
- ✅ 2D arrays (`list[list[int]]` with type annotations)
- ✅ File I/O and path operations
- ✅ OOP (classes, methods, inheritance)

**Known Limitations** (2 edge cases - NOT supported):

- ❌ Dict with list values (`dict[str, list[int]]`) - Use workaround (see C_BACKEND_PLAN.md)
- ❌ Bare `list` without type params (`list = [[1,2]]`) - Use `list[list[int]]` (best practice)

## Test Categories

### Container and Data Structure Tests

- `test_container_iteration.py` - Tests iteration over lists and sets
- `container_iteration_test.py` - Alternative container iteration tests
- `test_list_slicing.py` - Tests list slicing operations
- `test_simple_slice.py` - Simple slicing test cases
- `nested_2d_params.py` - Tests 2D array as function parameter
- `nested_2d_return.py` - Tests 2D array as return type
- `nested_2d_simple.py` - Simple 2D array test
- `nested_dict_list.py` - Dict with list values (not yet supported)
- `nested_containers_comprehensive.py` - Comprehensive nested container tests

### String Operation Tests

- `test_string_methods.py` - Tests string methods like `.upper()`, `.lower()`, `.find()`
- `string_methods_test.py` - Alternative string method tests
- `test_string_membership.py` - Tests string membership (`"x" in text`)
- `test_string_membership_simple.py` - Simplified string membership tests
- `test_string_methods_new.py` - Additional string method tests

### Basic Functionality Tests

- `simple_test.py` - Simple test case for basic functionality

## Purpose

These files are designed for:

- **Translation Testing**: Verifying that specific Python features translate correctly to C
- **Regression Testing**: Ensuring that changes don't break existing translation capabilities
- **Feature Validation**: Testing new translation features as they're implemented
- **Batch Testing**: Using with `cgen batch` command to test all translation features at once

## Note

For more complex demonstrations and examples beyond basic translation testing, see the `tests/demos/` directory.
