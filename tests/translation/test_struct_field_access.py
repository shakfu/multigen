"""Test struct field access for dataclass and namedtuple."""

from dataclasses import dataclass
from typing import NamedTuple


@dataclass
class Rectangle:
    width: int
    height: int


class Circle(NamedTuple):
    radius: float
    center_x: float
    center_y: float


def calculate_area(rect: Rectangle, circle: Circle) -> float:
    rect_area: int = rect.width * rect.height
    circle_area: float = 3.14159 * circle.radius * circle.radius
    return float(rect_area) + circle_area


def main() -> int:
    rect: Rectangle = Rectangle(10, 5)
    circle: Circle = Circle(3.0, 0.0, 0.0)
    area: float = calculate_area(rect, circle)
    return int(area)
