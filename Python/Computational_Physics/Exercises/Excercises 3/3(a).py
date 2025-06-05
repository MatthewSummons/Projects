# Compute the determinant, rank, inverse
#  and eigenpairs of a sqare matrix
# Created by Shaheer Ziya on 18/3/2022

import numpy as np
linalg = np.linalg

# Obtain the matrix you want to work with (3x3 in this case)
dimA = 3
A = np.array((
    (2, -1, 0),
    (3, 1, 2),
    (-1, 1, 1)
))

detA = linalg.det(A)
rankA = linalg.matrix_rank(A)
invA = linalg.inv(A)
eigVals, eigVecs = linalg.eig(A)

# Output the results to screen
print("For the following matrix: ")
print()
print(A)
print()

print(f"The determinant is {detA: .2f}")

print(f"The rank is {rankA}")

print("The inverse of the matrix is:")
print()
print(invA)
print()

print(f"|   Eigenvalues    |                 Eigenvectors")
for eigVal, eigVec in zip(eigVals, eigVecs):
    print(f"|{eigVal: .4f}", end="   ")
    print(f"|   {np.round(eigVec, 4)}")






