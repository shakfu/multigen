"""Demonstration of OOP features in MultiGen C backend."""

class Rectangle:
    def __init__(self, width: int, height: int):
        self.width: int = width
        self.height: int = height

    def area(self) -> int:
        return self.width * self.height

    def perimeter(self) -> int:
        return 2 * (self.width + self.height)

    def resize(self, new_width: int, new_height: int):
        self.width = new_width
        self.height = new_height

    def is_square(self) -> bool:
        return self.width == self.height

class Circle:
    def __init__(self, radius: int):
        self.radius: int = radius

    def area(self) -> int:
        # Simplified area calculation (π ≈ 3)
        return 3 * self.radius * self.radius

    def diameter(self) -> int:
        return 2 * self.radius

def test_shapes() -> int:
    # Create a rectangle
    rect: Rectangle = Rectangle(5, 10)
    rect_area: int = rect.area()

    # Create a circle
    circle: Circle = Circle(7)
    circle_area: int = circle.area()

    # Test methods
    rect.resize(8, 8)
    is_square: bool = rect.is_square()

    if is_square:
        return rect_area + circle_area
    else:
        return rect_area - circle_area