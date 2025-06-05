# VectorManip.py
# Calculate the sum, difference and dot product of two vectors
# Created by Shaheer Ziya

# Obtain the vectors from the user
# Remove the commas, brackets and ensure each number is followed by a whitespace (even trailing at the end)
textA = input("Enter the vector A: ").replace(",", "").strip("[]") + " "
textB = input("Enter the vector B: ").replace(",", "").strip("[]") + " "

# Ensure vectors are of the same length
while len(textA) != len(textB):
  print("Invalid input! Vectors A and B must have the same length.")
  textA = input("Enter the vector A: ").replace(",", "").strip("[]") + " "
  textB = input("Enter the vector B: ").replace(",", "").strip("[]") + " "

class Vec:
  def __init__(self, text) -> None:
    # Obtain entries of the vector from the given text
    num, self.entries = "", []    # Num temporarily stores the characters of the input number
    for char in text:
      if char != " ":
        num += char
      else:
        self.entries.append(int(num))
        num = ""
  
  
  def __str__(self) -> str:
      """Method to view the entries of the vector"""
      print(str(self.entries))
  
  
  def __add__(self, other: "Vec") -> str:
    """
    Obtain the entry-wise sum of two equi-length vectors
    """
    result = []
    for i in range(len(self.entries)):
      result.append(
        self.entries[i] + other.entries[i]
      )
    return str(result)
  
  
  def __sub__(self, other: "Vec") -> str:
    """
    Obtain the entry-wsie difference of two equi-length vectors
    """
    result = []
    for i in range(len(self.entries)):
      result.append(
          self.entries[i] - other.entries[i]
      )
    return str(result)
  
  
  def dot(self, other: "Vec") -> int:
    """
    Obtain the dot product of two-equi-length vectors
    """
    result = 0
    for i in range(len(self.entries)):
      result += (self.entries[i] * other.entries[i])
    return result

VecA = Vec(textA)
VecB = Vec(textB)

print(f"A + B = {VecA + VecB}")
print(f"A - B = {VecA - VecB}")
print(f"A . B = {VecA.dot(VecB)}")


