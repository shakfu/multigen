"""
Test any() and all() built-in functions across backends.
"""

import pytest
import tempfile
from pathlib import Path
from multigen.pipeline import MultiGenPipeline


def test_any_basic_cpp():
    """Test basic any() function in C++."""
    code = '''
def test_any() -> bool:
    values: list[bool] = [False, False, True, False]
    return any(values)

def main() -> None:
    result: bool = test_any()
    print(result)
'''

    with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(code)
        temp_file = f.name

    try:
        with tempfile.TemporaryDirectory() as temp_dir:
            pipeline = MultiGenPipeline(target_language="cpp")
            result = pipeline.convert(temp_file, temp_dir)

            assert result.success, f"Conversion failed: {result.errors}"
            assert "multigen::any" in Path(result.generated_files[0]).read_text()
    finally:
        Path(temp_file).unlink()


def test_all_basic_cpp():
    """Test basic all() function in C++."""
    code = '''
def test_all() -> bool:
    values: list[bool] = [True, True, True, True]
    return all(values)

def main() -> None:
    result: bool = test_all()
    print(result)
'''

    with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(code)
        temp_file = f.name

    try:
        with tempfile.TemporaryDirectory() as temp_dir:
            pipeline = MultiGenPipeline(target_language="cpp")
            result = pipeline.convert(temp_file, temp_dir)

            assert result.success, f"Conversion failed: {result.errors}"
            assert "multigen::all" in Path(result.generated_files[0]).read_text()
    finally:
        Path(temp_file).unlink()


def test_any_all_combined_cpp():
    """Test combined any() and all() usage."""
    code = '''
def check_conditions() -> bool:
    has_any_true: list[bool] = [False, True, False]
    all_true: list[bool] = [True, True, True]

    if any(has_any_true) and all(all_true):
        return True
    return False

def main() -> None:
    result: bool = check_conditions()
    print(result)
'''

    with tempfile.NamedTemporaryFile(mode="w", suffix=".py", delete=False) as f:
        f.write(code)
        temp_file = f.name

    try:
        with tempfile.TemporaryDirectory() as temp_dir:
            pipeline = MultiGenPipeline(target_language="cpp")
            result = pipeline.convert(temp_file, temp_dir)

            assert result.success, f"Conversion failed: {result.errors}"
            generated_code = Path(result.generated_files[0]).read_text()
            assert "multigen::any" in generated_code
            assert "multigen::all" in generated_code
    finally:
        Path(temp_file).unlink()


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
