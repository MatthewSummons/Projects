# Mass, Center of Mass, and Moment of Inertia of a Laminar
# Created by Shaheer Ziya

import matplotlib.pyplot as plt
import scipy.integrate as intgr
import numpy as np

# Bounds for the lamina
x = 2
y = x * np.exp(-x)

# The density function for the lamina
def sigma(x, y):
  return (x * y) ** 2

# Functions for Centre of Mass
def xsigma(x, y):
  return x * sigma(x, y)
def ysigma(x, y):
  return y * sigma(x, y)
# Functions for Moments of Inertia
def x2sigma(x, y):
  return (x ** 2) * sigma(x, y)
def y2sigma(x, y):
  return (y ** 2) * sigma(x, y)

def main():
  
  M = intgr.dblquad(sigma, 0, x, 0, y)[0]
  xm = (1/M) * intgr.dblquad(xsigma, 0, x, 0, y)[0]
  ym = (1/M) * intgr.dblquad(ysigma, 0, x, 0, y)[0]
  Ix = intgr.dblquad(y2sigma, 0, x, 0, y)[0] 
  Iy = intgr.dblquad(x2sigma, 0, x, 0, y)[0]
  
  print(f"The mass of the lamina is ~{M:.3f} kg")
  print(f"The centre of mass of the lamina is ({xm:.2f}, {ym:.2f}) (upto 2 d.p. in metres)")
  print(f"The moments of inertia of the lamina are {Ix:.5f} kgm^2 and {Iy:.5f} kgm^2")


main()