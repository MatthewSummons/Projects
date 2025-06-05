# PHYS2160 Introductory Computational Physics
Introduction to Object Oriented Programming and Numerical Analysis (with numpy and matplotlib).
Please refer to the course outline for more details about the course and its schedule.

Below are examples of pieces of some notable programs.

## Bubble Sort Implementation
```python
def bubbleSort(arr):
    n = len(arr)
    print("Sorting Process of a List of Integers by Bubble Sort: ")
    print()
    
    # Traverse through all array elements
    for i in range(n-1):
        # Last i elements are already in place
        for j in range(0, n-i-1):
            # Traverse the array from 0 to n-i-1
            # Swap if the element found is greater than the next element
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
            print(arr)

```

## 3D Vectors Class

```python
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
```
