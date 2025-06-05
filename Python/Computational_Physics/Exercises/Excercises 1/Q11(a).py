# Q11(a).py
# Create a numpy array where the i-j-th entry is i*j (for i,j starting at 1)
# Created by Shaheer Ziya on 21-02-2022 07:10 UTC

import numpy as np


def main():
    m = int(input("How many rows does the array have: "))
    n = int(input("How many columns does the array have: "))

    # Anonymous function which returns the product of the indices offset by 1 (b/c i,j≠0)
    array = np.fromfunction(lambda i, j: (i+1) * (j+1), (m,n))
    # Convert entries to integers
    array = np.array(array, dtype = int)
    # Output array to standard output
    print(array)

main()