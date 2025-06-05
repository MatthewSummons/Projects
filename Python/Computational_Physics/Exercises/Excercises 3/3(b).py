# 3(b).py
# Program to solve linear equations unsing linalg in numpy
# Created by Shaheer Ziya

import numpy as np
linalg = np.linalg

def main():
  #! 3b) i)
  
  # Coefficient Matrix for the system of equations
  A = np.array([
    (-3, -2, 4),
    (0, 3, -2),
    (4, -3, 2)
  ])
  # Target Vector
  b = np.vstack(np.array((9, 5, 7))) # Convert to a column vector
  # Ax = b
  Sol = linalg.solve(A, b)
  
  print("The solution to the first system Ax = b is: ")
  print(Sol)
  print()

  # Coefficient Matrix for the system of equations
  B = np.array([
    (1, 1, 1, 1),
    (1, 2, 3, 4),
    (1, 3, 6, 10),
    (1, 4, 10, 20)
  ])
  # Target Vector
  c = np.vstack(np.array((4, 10, 20, 35))) # Convert to a column vector
  # Bx = c
  Sol = linalg.solve(B, c)
  
  print("The solution to the second sytem Bx = c is: ")
  print(Sol)
  print()


main()