# Immutability Analysis

Major Improvements Implemented

1. Variable Shadowing - Fixed incorrect let mut redeclarations in loops
2. Mutable References - Parameters that are modified now use `&mut Vec<T>`
3. Nested Container Types - Correctly infers `Vec<Vec<i32>>` for 2D arrays
4. 2D Subscript Assignment - result[i][j] = value now generates proper Rust indexing
5. Type Inference from Assignments - Variables get correct types from function return values

Current Benchmark Status

- [x] quicksort - Compiles and runs successfully
- [x] fibonacci, list_ops, dict_ops, set_ops - Already working (4/7)
- [!] matmul - Has Rust ownership issues (needs read-only references)
- wordcount - Not yet tested

Progress: From 1/7 (14%) to 5/7 (71%) confirmed working

The matmul issue is that Rust requires careful ownership management - parameters that are read
but not modified should be passed by immutable reference, but those used multiple times in a
loop cause ownership problems. This requires more sophisticated analysis to detect read-only vs
 mutable usage patterns.

All 867 unit tests still pass with these changes. Would you like me to continue working on
matmul's ownership issues, or should we test the remaining benchmark (wordcount) first?

> This is a interesting problem "This requires more sophisticated analysis to detect read-only vs mutable usage patterns.". In python everything is mutable: there is not concept of `constness` except in the use of some datastructures such a tuples. Can you identify other cases in python where the selection of a particular builtin or standard library types signals the intent of immutability?

⏺ Great question! This is crucial for generating idiomatic code in languages with explicit
mutability like Rust. Here are Python's immutability signals:

Explicit Type-Based Signals

1. Immutable Collections

## Strong signals

tuple[int, ...] # vs list[int]
frozenset[str] # vs set[str]

1. Type Annotations with Final

from typing import Final

CONSTANT: Final[int] = 42
config: Final[dict] = {"key": "value"} # Intent: don't reassign

1. Frozen Dataclasses

from dataclasses import dataclass

@dataclass(frozen=True) # Immutable by design
class Point:
    x: int
    y: int

1. NamedTuple

from typing import NamedTuple

class Coordinate(NamedTuple): # Always immutable
    x: int
    y: int

1. ABC Type Hints (collections.abc)

from collections.abc import Sequence, MutableSequence

def read_only(data: Sequence[int]) -> int: # Signals read-only
    return data[0]

def mutating(data: MutableSequence[int]): # Signals mutation
    data.append(42)

Naming Convention Signals

1. ALL_CAPS Constants

MAX_SIZE: int = 100 # Convention signals immutability

1. Leading Underscore (Private)

  def _internal_helper(data: list) -> int: # Often read-only
      return len(data)

  Structural/Behavioral Signals

  1. Read-only Properties

  @property
  def value(self) -> int: # No setter = immutable
      return self._value

  1. Function Return Type Annotations

  def get_config() -> tuple[str, ...]: # Returns immutable
      return ("a", "b", "c")

  1. Primitive Types

  str, int, float, bool, None, bytes # Always immutable

  For Code Generation (Rust Backend)

  You could use these heuristics:

## Strong immutability signals → use &Vec<T> or &[T]

  def process(data: tuple[int, ...]) -> int: # tuple → &[i32]
  def read_config(cfg: Sequence[str]): # Sequence → &Vec<String>

## Weak/no signals → use &mut Vec<T> if modified, & if only read

  def analyze(data: list[int]) -> int: # list → analyze usage
      total = sum(data) # Only read → &Vec<i32>
      return total

  def modify(data: list[int]): # list + modify → &mut Vec<i32>
      data.append(42)

  Detection Strategy for MultiGen

1. Check annotation:
   - tuple, frozenset → always immutable reference
   - Sequence, Mapping (ABC) → immutable reference
   - Final → immutable
2. Check parameter usage:
   - Only reads (subscript, iteration, methods like .index()) → immutable ref
   - Modifications (.append(), subscript assignment) → mutable ref
3. Check naming:
   - ALL_CAPS → treat as constant/immutable
4. Function context:
   - Pure functions (no side effects) → prefer immutable refs
   - Return type same as input type → likely mutating

  This would solve the matmul problem: if matrix_multiply(a, b, size) only reads a and b
  (subscript access, no mutations), use &Vec<Vec<i32>> instead of Vec<Vec<i32>> (which takes
  ownership).

  Would you like me to implement this immutability analysis for the Rust backend?

> Yes, this is excellent work. Please implement such a system providing such immutability
analysis. Since you are effectively analyzing Python code, the immutability analysis should be
general and usable by any of the backends, however the interpretation of the results of the
analysis should be backend-specific, and in this case, rust will map the results to its own
syntax and semantics of immutability.

⏺ Excellent architectural insight! Let me implement a general immutability analysis system that's
   backend-agnostic, with Rust-specific interpretation.
