# Q4(a).py
# Classes representing 2D & 3D Vectors
# Created by Shaheer Ziya on UTC+08 15:09

import math
class Vec2D:
    """Class Representing 2D Vectors"""

    def __init__(self, xCord, yCord) -> None:
        """Initialize Vector with x,y co-ordinates.
        Elements are assumed to be contained in the set of all real numbers."""
        self.x = xCord
        self.y = yCord
    

    def __str__(self: "Vec2D") -> str:
        return f"({self.x}, {self.y})^T"


    def __add__ (self: "Vec2D", other: "Vec2D") -> "Vec2D":
        "Return the elementwise sum of two 2D vectors"
        return Vec2D(
            (self.x + other.x),
            (self.y + other.y)
        )
    
    def __sub__(self: "Vec2D", other: "Vec2D") -> "Vec2D":
        "Return the elementwise subtaction of two 2D vectors : Self - Other"
        return Vec2D(
            (self.x - other.y),
            (self.y - other.y)
        ) 
    

    def __mul__(self: "Vec2D", other: "Vec2D") -> float:
        "Return the dot product of two 2D vectors"
        return (self.x * other.x) + (self.y * other.y)
    

    def __eq__(self: "Vec2D", other: "Vec2D") -> bool:
        "Check if two 2D vectors are equivalent"
        return (
            math.isclose(self.x, other.x) and math.isclose(self.y, other.y)
        )
    

    # def __neq__(self: "Vec2D", other: "Vec2D") -> bool:
    #   """Check if two 2D vectors are unequal"""  
    #   return not (self.__eq__(other))
    

    def __abs__(self: "Vec2D") -> float:
        "Return the Euclidean Norm of the vector"
        return math.sqrt( (self.x * self.x) + (self.y * self.y) )


class Vec3D(Vec2D):
    """Class representing 3D vectors, inherits from the Vec2D class"""

    def __init__(self, xCord, yCord, zCord) -> None:
        """Initialize vector with x,y,z co-ordinates"""
        super().__init__(xCord, yCord)
        self.z = zCord
    
    def __str__(self: "Vec3D") -> str:
        return f"({self.x}, {self.y}, {self.z})^T"
    

    def __add__(self: "Vec3D", other: "Vec3D") -> "Vec3D":
        """Return the elementwise sum of two 3D vectors"""
        return Vec3D(
            (self.x + other.x),
            (self.y + other.y),
            (self.z + other.z)
        )
    

    def __sub__(self: "Vec3D", other: "Vec3D") -> "Vec3D":
        """Return the elementwise subratction of two 3D vectors"""
        return Vec3D(
            (self.x - other.x),
            (self.y - other.y),
            (self.z - other.z)
        )
    

    def __mul__(self: "Vec3D", other: "Vec3D") -> float:
        """Return the dot product of two 3D vectors"""
        return (
            (self.x * other.x) + (self.y * other.y) + (self.z * other.z)
        )
    

    def __eq__(self: "Vec3D", other: "Vec3D") -> bool:
        """Check if two 3D vectors are equivalent"""
        # Why does this work???
        return (super().__eq__(other) and (math.isclose(self.z, other.z)))
    

    def __abs__(self: "Vec3D") -> float:
        """Return the Euclidean Norm of the 3D vector"""
        # ||V|| = sqrt( (sqrt(x^2 + y^2))^2 + z^2 )
        return math.sqrt(math.pow((super().__abs__()), 2) + (self.z * self.z))
    

    def __cross__(self: "Vec3D", other: "Vec3D") -> "Vec3D":
        """Return the cross-product of two 3D vectors"""
        return Vec3D(
            ((self.y * other.z) - (other.y * self.z)),
            ((other.x * self.z) - (self.x * other.z)),
            ((self.x * other.y) - (other.x * self.y))
        )


a = Vec3D(0,0,0)
b = Vec3D(0,0,0)
print(a.__eq__(b))
print(a.__abs__())

a2 = Vec3D(1,0,0)
b2 = Vec3D(1,0,1)
print(b2.__abs__())
print(a2.__eq__(b2))
print(a2.__cross__(b2))
