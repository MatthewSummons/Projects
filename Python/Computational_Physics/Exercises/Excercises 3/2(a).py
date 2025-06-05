# Traditional Rate of Change Question
# Created by Shaheer Ziya on 17/3/2022 UTC+08 15:25

import matplotlib.pyplot as plt
import numpy as np
Polynomial = np.polynomial.Polynomial

# R := Radius of the Sphere, F = - dV/dt (constant)
R , F = 1.5, 2.0e-4
# Initial Volume
V0 = (4/3) * np.pi * R**3

# Total time to take to empty the tank
T = V0 / F

# Coefficients for the polynomial
h2 = np.pi * R
h3 = -(1/3) * np.pi

# Solve p(h) = 0 to find the value of h from time t = 0 to t = T
N = 100                         # Number of iterations
time = np.linspace(0, T, N)     # sth
h = np.ones(N) * 2 * R          # Diameter
# print(N, time, h, sep="\n")




# h = np.polynomial.Polynomial([]) 