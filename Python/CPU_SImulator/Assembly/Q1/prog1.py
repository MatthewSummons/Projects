# prog1.py
# High-Level Code for prog1 (Assembly)
# Created by Shaheer Ziya on Apr 9, 2022 UTC+08 19:39

# Implement the stack using a python list
stack = []
# Initialize Registers 11, 12 & 13
R11, R12, R13 = 0, "Some Important Value", "Maybe even Grabage"


def SQ(R10: int) -> int:
  '''Square the passed value and return it'''
  global R11, R12, R13
  stack.append(R12)
  stack.append(R13)

  R13 = 1     # P1 = 1
  R11 -= R11  # Initialize R11 to 0
  R12 = R10   # R10 is the number of loop in the main function we are in

  while (R12 != 0):
    R11 += R10  # Add R10 to R11
    R12 -= R13  # Decrement R12
  
  R13 = stack.pop()
  R12 = stack.pop()

  return R11


def main():
  # Initialize Registers with neccesary values
  R4 = 0    # P1 = 0
  R1 = 1    # P2 = 1
  R2 = R1   # R2 = 1
  R3 = 0xA

  R11 = 0

  # R5 determines how many iterations left
  R5 = R3 - R1
  # Condition in while loop acts as BNZ instruction
  while (R5 != 0):
    R10 = R1
    R11 = SQ(R10)
    R4 += R11 # Accumulate the results of each iteration (R11) into R4
    R1 += R2      # Increment R1
    R5 = R3 - R1 # Update R5
  
  # Store the final result in the last word of the program
  P = R4

  # Output the result for convenience
  print(f"The final value is {P}")

main()