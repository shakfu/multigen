"""Tests for LLVM backend string methods."""

import subprocess
import tempfile
from pathlib import Path

import pytest


def compile_and_run_c_test(c_code: str) -> str:
    """Compile and run C code that tests string methods.

    Args:
        c_code: C code to compile and run

    Returns:
        Output from the program
    """
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)

        # Write C code
        c_file = temp_path / "test.c"
        c_file.write_text(c_code)

        # Find string runtime
        runtime_path = Path(__file__).parent.parent / "src" / "multigen" / "backends" / "llvm" / "runtime"
        string_c = runtime_path / "multigen_llvm_string.c"
        string_h = runtime_path / "multigen_llvm_string.h"

        if not string_c.exists():
            pytest.skip(f"String runtime not found: {string_c}")

        # Compile
        output_file = temp_path / "test"
        compile_cmd = [
            "clang",
            "-I", str(runtime_path),
            str(c_file),
            str(string_c),
            "-o", str(output_file)
        ]

        result = subprocess.run(compile_cmd, capture_output=True, text=True)
        if result.returncode != 0:
            pytest.fail(f"Compilation failed: {result.stderr}")

        # Run
        run_result = subprocess.run([str(output_file)], capture_output=True, text=True, timeout=5)
        if run_result.returncode != 0:
            pytest.fail(f"Execution failed (exit {run_result.returncode}): {run_result.stderr}")

        return run_result.stdout.strip()


class TestStringJoin:
    """Test multigen_str_join() function."""

    def test_join_basic(self):
        """Test basic string join with comma separator."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    multigen_string_array_t* arr = multigen_string_array_new();
    multigen_string_array_add(arr, multigen_strdup("apple"));
    multigen_string_array_add(arr, multigen_strdup("banana"));
    multigen_string_array_add(arr, multigen_strdup("cherry"));

    char* result = multigen_str_join(", ", arr);
    printf("%s", result);

    free(result);
    multigen_string_array_free(arr);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "apple, banana, cherry"

    def test_join_empty_separator(self):
        """Test join with empty separator."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    multigen_string_array_t* arr = multigen_string_array_new();
    multigen_string_array_add(arr, multigen_strdup("hello"));
    multigen_string_array_add(arr, multigen_strdup("world"));

    char* result = multigen_str_join("", arr);
    printf("%s", result);

    free(result);
    multigen_string_array_free(arr);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "helloworld"

    def test_join_single_element(self):
        """Test join with single element."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    multigen_string_array_t* arr = multigen_string_array_new();
    multigen_string_array_add(arr, multigen_strdup("only"));

    char* result = multigen_str_join(", ", arr);
    printf("%s", result);

    free(result);
    multigen_string_array_free(arr);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "only"

    def test_join_empty_array(self):
        """Test join with empty array."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    multigen_string_array_t* arr = multigen_string_array_new();

    char* result = multigen_str_join(", ", arr);
    printf("%s", result);

    free(result);
    multigen_string_array_free(arr);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == ""


class TestStringReplace:
    """Test multigen_str_replace() function."""

    def test_replace_basic(self):
        """Test basic string replacement."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_replace("hello world", "world", "python");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "hello python"

    def test_replace_multiple_occurrences(self):
        """Test replacing multiple occurrences."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_replace("foo bar foo baz foo", "foo", "test");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "test bar test baz test"

    def test_replace_no_match(self):
        """Test replace when substring not found."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_replace("hello world", "xyz", "abc");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "hello world"

    def test_replace_with_longer_string(self):
        """Test replacing with longer string."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_replace("hi", "hi", "hello there");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "hello there"

    def test_replace_with_empty_string(self):
        """Test replacing with empty string (deletion)."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_replace("hello world", " world", "");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "hello"


class TestStringUpper:
    """Test multigen_str_upper() function."""

    def test_upper_basic(self):
        """Test basic uppercase conversion."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_upper("hello world");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "HELLO WORLD"

    def test_upper_mixed_case(self):
        """Test uppercase with mixed case."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_upper("HeLLo WoRLd");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "HELLO WORLD"

    def test_upper_with_numbers(self):
        """Test uppercase with numbers and special chars."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* result = multigen_str_upper("test123!@#");
    printf("%s", result);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "TEST123!@#"


class TestStringStartsWith:
    """Test multigen_str_startswith() function."""

    def test_startswith_true(self):
        """Test startswith when prefix matches."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>

int main() {
    int result = multigen_str_startswith("hello world", "hello");
    printf("%d", result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "1"

    def test_startswith_false(self):
        """Test startswith when prefix doesn't match."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>

int main() {
    int result = multigen_str_startswith("hello world", "world");
    printf("%d", result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "0"

    def test_startswith_empty_prefix(self):
        """Test startswith with empty prefix."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>

int main() {
    int result = multigen_str_startswith("hello", "");
    printf("%d", result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "1"


class TestStringEndsWith:
    """Test multigen_str_endswith() function."""

    def test_endswith_true(self):
        """Test endswith when suffix matches."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>

int main() {
    int result = multigen_str_endswith("hello world", "world");
    printf("%d", result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "1"

    def test_endswith_false(self):
        """Test endswith when suffix doesn't match."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>

int main() {
    int result = multigen_str_endswith("hello world", "hello");
    printf("%d", result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "0"

    def test_endswith_full_string(self):
        """Test endswith with full string as suffix."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>

int main() {
    int result = multigen_str_endswith("test", "test");
    printf("%d", result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "1"


class TestStringIntegration:
    """Integration tests combining multiple string methods."""

    def test_split_and_join(self):
        """Test splitting then joining strings."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    // Split
    multigen_string_array_t* parts = multigen_str_split("one,two,three", ",");

    // Join with different separator
    char* result = multigen_str_join(" - ", parts);
    printf("%s", result);

    free(result);
    multigen_string_array_free(parts);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "one - two - three"

    def test_upper_and_replace(self):
        """Test combining upper and replace."""
        c_code = '''
#include "multigen_llvm_string.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
    char* upper = multigen_str_upper("hello world");
    char* result = multigen_str_replace(upper, "WORLD", "PYTHON");
    printf("%s", result);

    free(upper);
    free(result);
    return 0;
}
'''
        output = compile_and_run_c_test(c_code)
        assert output == "HELLO PYTHON"
