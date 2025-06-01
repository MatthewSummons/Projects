# prog2.py
# High-Level code for Prog2 (Assembly)
# Created by Shaheer Ziya on UTC+08 17:12

# Load values from memory to registers
R4 = 0
R1 = 1
R2 = 5
R3 = 4

# GoTo (loop) with Branch Condition R3 != 0
while (R3 != 0):
    R4 += R2        # Add R2 to R4 (initially 0) 4 times (the initial value of R3)
    R3 -= R1        # Decrement R3; R3 is the number of iterations left
# Halt Program

# Store value of Register 4 to Memory
print(f"{R2} x 4 = {R4}")

# The program calculates R2 x R3 & writes the result to memory

