"""Test functional quicksort translation for Haskell backend."""

import pytest

from multigen.backends.haskell.converter import MultiGenPythonToHaskellConverter
from multigen.backends.errors import UnsupportedFeatureError


class TestHaskellFunctionalQuicksort:
    """Test functional quicksort without array mutations."""

    def setup_method(self):
        """Set up test fixtures."""
        self.converter = MultiGenPythonToHaskellConverter()

    def test_functional_quicksort(self):
        """Test that functional quicksort (without mutations) translates correctly."""
        python_code = """
def quicksort(arr: list) -> list:
    if len(arr) <= 1:
        return arr

    pivot: int = arr[0]
    rest: list = arr[1:]

    less: list = [x for x in rest if x < pivot]
    greater: list = [x for x in rest if x >= pivot]

    return quicksort(less) + [pivot] + quicksort(greater)
"""

        result = self.converter.convert_code(python_code)

        # Should compile without mutation errors
        assert "quicksort" in result
        assert "::" in result  # Should have type signature
        assert "drop" in result  # Should use drop for arr[1:]
        # Should not raise UnsupportedFeatureError about mutations
        assert "mutates array" not in result.lower()

    def test_functional_quicksort_with_main(self):
        """Test complete functional quicksort program."""
        python_code = """
def quicksort(arr: list) -> list:
    if len(arr) <= 1:
        return arr

    pivot: int = arr[0]
    rest: list = arr[1:]
    less: list = [x for x in rest if x < pivot]
    greater: list = [x for x in rest if x >= pivot]

    return quicksort(less) + [pivot] + quicksort(greater)

def main() -> int:
    arr: list = [3, 1, 4, 1, 5, 9, 2, 6]
    sorted_arr: list = quicksort(arr)
    print(sorted_arr[0])
    return 0
"""

        result = self.converter.convert_code(python_code)

        # Should compile successfully
        assert "quicksort" in result
        assert "main :: IO ()" in result
        assert "mutates array" not in result.lower()

    def test_imperative_quicksort_raises_error(self):
        """Test that imperative quicksort (with mutations) raises appropriate error."""
        python_code = """
def quicksort(arr: list, low: int, high: int) -> int:
    if low < high:
        pivot: int = arr[high]
        i: int = low - 1

        for j in range(low, high):
            if arr[j] <= pivot:
                i += 1
                temp: int = arr[i]
                arr[i] = arr[j]
                arr[j] = temp

        temp2: int = arr[i + 1]
        arr[i + 1] = arr[high]
        arr[high] = temp2

        pi: int = i + 1

        quicksort(arr, low, pi - 1)
        quicksort(arr, pi + 1, high)

    return 0
"""

        with pytest.raises(UnsupportedFeatureError) as exc_info:
            self.converter.convert_code(python_code)

        # Should mention array mutations
        assert "mutates array" in str(exc_info.value).lower()
        assert "arr" in str(exc_info.value)
