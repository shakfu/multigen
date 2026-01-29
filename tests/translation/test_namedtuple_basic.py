"""Basic namedtuple to C struct conversion test."""

from typing import NamedTuple


class Coordinate(NamedTuple):
    x: int
    y: int


class Student(NamedTuple):
    name: str
    grade: int
    score: float


def main() -> int:
    coord: Coordinate = Coordinate(5, 10)
    student: Student = Student("Bob", 12, 95.5)
    return 0
