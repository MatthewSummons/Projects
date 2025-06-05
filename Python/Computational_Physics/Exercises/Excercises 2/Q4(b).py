# Q4(b).py
# Classes representing 1D dots, 2D circles & 3D cyliders
# Created by Shaheer Ziya on UTC+08 16:28

import math     # To use the value of pi
class Point:
    """A class representing points in 2D"""

    def __init__(self, xCord: int, yCord: int) -> None:
        """Initialize point with integer x,y coordinates"""
        self.x = xCord
        self.y = yCord
    

    def __str__(self) -> str:
        return f"({self.x}, {self.y})"

    
    # def __repr__(self) -> str:
    #     return f"({self.x}, {self.y})"

# A = Point(1,2)
# print(A)

class Circle(Point):
    """"A class representing circles (in 2D) by inheriting from the Point class"""

    def __init__(self, xCord: int, yCord: int, radius: float) -> None:
        """Initialize a Circle with a centre (x,y) and radius"""
        super().__init__(xCord, yCord)
        self.centre = super().__str__()
        self.radius = radius


    def __str__(self):
        return f"Centre = {self.centre}, Radius = {self.radius}"
    

    def area(self) -> float:        
        """Return the area of the circle"""
        return math.pi * (self.radius * self.radius)


# B = Circle(0,0,5)
# print(B)
# print(B.area())

class Cylinder(Circle):
    """A class representing cylinders in 3D by inheriting from the Circle class"""

    def __init__(self, xCord: int, yCord: int, radius: float, height: float) -> None:
        """Initialize a cylinder with a base circle centered at (x,y) with some radius,
        and a height"""
        super().__init__(xCord, yCord, radius)
        self.height = height
    

    def __str__(self):
        return super().__str__() + f", Height = {self.height}"
    

    def volume(self) -> float:
        """Return the volume of the cylinder"""
        # Volume = Base Area x Perpendicular Height
        return super().area() * self.height
    
    def area(self) -> float:
        """Return the surface area of the cylinder"""
        # SA = 2piR^2 + 2piRH
        return (2 * super().area()) + (2 * math.pi * self.radius * self.height)

# C = Cylinder(0,0,5,5)
# print(C)
# print(C.volume())
# print(C.area())