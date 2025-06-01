# prog.py
# High Level Code of prog (Assembly)
# Created by Shaheer Ziya on 24-02-2022 UTC+08 16:03

# Load values from memory to registers
R4 = 0
R1 = 1
R2 = R1
R3 = 10

# Initialize R5 w/out declaring it
R5 = None
# GoTo (loop) with Branch Condition R5 != 0
while (R5 != 0):
    R4 += R1         # Add value of Register 1 to Register 4
    R1 += 1          # Increment Register 1
    R5 = 10 - R1     # Store how many iterations left
# Halt Program

# Store value of Register 4 to Memory
print(
    f"The program calculates the sum 1 + 2 + ... + {R3 - 2} + {R3 - 1} = {R4}")
# Prints 'The program calculates the sum 1 + 2 + ... + 8 + 9 = 45'
