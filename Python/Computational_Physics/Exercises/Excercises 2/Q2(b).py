# Q2(b).py
# Created by Shaheer Ziya on UTC+08 17:04

import math

class RationalNumber:
    """A Class representing numbers in the set {P/Q : P,Q ∈ ℤ}"""
    
    
    def __init__(self, numerator: int, denominator: int) -> None:
        """Stores the reduced form of fraction"""
        # Check if numerator & denominator are integers
        # Reduce to simplest form if not already
        commonFactor = math.gcd(numerator, denominator)
        if commonFactor != 1:
            numerator /= commonFactor
            denominator /= commonFactor  
        
        self.num = numerator    
        self.den = denominator
    
    
    def __add__(self: "RationalNumber", other: "RationalNumber") -> "RationalNumber":
        """Add two rational numbers together & return the result in reduced form"""
        # a/b + c/d = (ad + bc) / bd
        return RationalNumber(
            (self.num * other.den) + (self.den * other.num),     # Numerator of result
            (self.den * other.den))                              # Denominator of result
    

    def __sub__(self: "RationalNumber", other: "RationalNumber") -> "RationalNumber":
        # a/b + c/d = (ad + bc) / bd
        return RationalNumber(
            (self.num * other.den) - (self.den * other.num),     # Numerator of result
            (self.den * other.den))                              # Denominator of result
    

    def __mul__(self: "RationalNumber", other: "RationalNumber") -> "RationalNumber":
        # a/b x c/d = ac/ bd
        return RationalNumber(
            (self.num * other.num),                              # Numerator of result
            (self.den * other.den))                              # Denominator of result
    

    def __truediv__(self: "RationalNumber", other: "RationalNumber") -> "RationalNumber":
        # a/b ÷ c/d = a/b x d/c
        return RationalNumber(
            (self.num * other.den),                               # Numerator of result
            (self.den * other.num)                                # Denominator of result
        )


    def display(self) -> None:
        print(self)


    def __str__(self) -> str:
        return f"{self.num}/{self.den}"

x1 = RationalNumber(22,7)
x2 = RationalNumber(7,22)

x1.display()
print(x1.__mul__(x2))