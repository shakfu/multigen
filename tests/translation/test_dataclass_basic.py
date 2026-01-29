"""Basic dataclass to C struct conversion test."""

from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


@dataclass
class Person:
    name: str
    age: int
    height: float


def main() -> int:
    p: Point = Point(10, 20)
    person: Person = Person("Alice", 30, 5.5)
    return 0
