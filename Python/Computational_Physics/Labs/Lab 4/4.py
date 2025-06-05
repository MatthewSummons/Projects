# Quadrilaterals, Trapezoids, Parallelograms, and Squares
# Created by Shaheer Ziya

# Import the Point class from the file point.py
from point import Point

# A class representing a geometric quadrilateral
class Quadrilateral:
  
  def __init__(self, *args) -> None:
    # Define the four points of the quadrilateral
    self.pt1 = Point(args[0], args[1])
    self.pt2 = Point(args[2], args[3])
    self.pt3 = Point(args[4], args[5])
    self.pt4 = Point(args[6], args[7])
  

  ### Methods to retrieve the coordinates of the quadrilateral ###
  def getpt1(self) -> Point:
    """ Return the first point of the quadrilateral """
    return self.pt1
  
  def getpt2(self) -> Point:
    """ Return the second point of the quadrilateral """
    return self.pt2
  
  def getpt3(self) -> Point:
    """ Return the third point of the quadrilateral """
    return self.pt3
  
  def getpt4(self) -> Point:
    """ Return the fourth point of the quadrilateral """
    return self.pt4
  

  def getcoorstr(self) -> str:
    """ Return a string representation of the coordinates of the quadrilateral """
    return f"{self.pt1}, {self.pt2}, {self.pt3}, {self.pt4}"

  
  def __str__(self) -> str:
    """ Return a string representation of the quadrilateral """
    return f"Coordinates of the Quadrilateral are:\n{self.getcoorstr()}"

# A class representing a geometric trapezoid
class Trapezoid(Quadrilateral):

  def __init__(self, *args) -> None:
    # Call the constructor of the Quadrilateral class
    super().__init__(*args)
  

  def getheight(self) -> float:
    """ Return the height of the trapezoid """
    # It has been assumed that the points will be entered in a cyclic order
    # And that the parallel sides of the trapezoiod will be parallel to the x-axis
    return self.pt3.gety() - self.pt1.gety()


  def getsumoftwosides(self) -> float:
    """ Return the sum of the two parallel sides of the trapezoid """
    # It has been assumed that the points will be entered in a cyclic order
    # And that the parallel sides of the trapezoiod will be parallel to the x-axis
    return (self.pt2.getx() - self.pt1.getx()) + (self.pt3.getx() - self.pt4.getx())

  
  def getarea(self) -> float:
    """ Return the area of the trapezoid """
    return (self.getheight() * self.getsumoftwosides()) / 2
  

  def __str__(self) -> str:
      # Adjust for the first line of the string representation
      return f"Coordinates of the Trapezoid are:\n{super().__str__()[38:]}" + "\n" + \
        f"Height = {self.getheight()}" + "\n" + \
        f"Area = {self.getarea()}"


# A class representing a geometric parallelogram
class Parallelogram(Trapezoid):

  def __init__(self, *args) -> None:
      super().__init__(*args)
  

  def getwidth(self) -> float:
    """ Return the width of the parallelogram """
    # It has been assumed that the points will be entered in a cyclic order
    # And that the parallel sides of the parallelogram will be parallel to the x-axis
    return self.pt2.getx() - self.pt1.getx()
  

  def __str__(self) -> str:
      # Insert the width part into the original string representation
      superiorLine = super().__str__()[34:]
      newline = superiorLine.find("\n")
      Cord_line = superiorLine[:newline]
      Area_line = superiorLine[superiorLine.find("\nHeight"):]
      # Area_Line = 
      return "Coordinates of the Parallelogram are:\n" + Cord_line +  f"\nWidth = {self.getwidth()}" + \
        Area_line


# A class representing a geometric square
class Square(Parallelogram):
  
    def __init__(self, *args) -> None:
      super().__init__(*args)
    
  
    def getside(self) -> float:
      """ Return the side of the square """
      # It has been assumed that the points will be entered in a cyclic order
      # And that the parallel sides of the square will be parallel to the x-axis
      return self.pt2.getx() - self.pt1.getx()
  
  
    def __str__(self) -> str:
      superiorLine = super().__str__()[38:]
      Cord_line = superiorLine[:superiorLine.find("\n")]
      Area_line = superiorLine[superiorLine.find("\nArea"):]
      return f"Coordinates of the Square are:\n" + Cord_line + f"\nSide = {self.getside()}" + Area_line


def main():
  
  a = Quadrilateral(1.1, 1.2, 6.6, 2.8, 6.2, 9.9, 2.2, 7.4)
  print(a)

  print()

  b = Trapezoid(0.0, 0.0, 10.0, 0.0, 8.0, 5.0, 3.3, 5.0)
  print(b.getsumoftwosides())
  print(b)

  print()

  c = Parallelogram(5.0, 5.0, 11.0, 5.0, 12.0, 20.0, 6.0, 20.0)
  print(c)

  print()

  d = Square(4.0, 0.0, 8.0, 0.0, 8.0, 4.0, 4.0, 4.0)
  print(d)

main()