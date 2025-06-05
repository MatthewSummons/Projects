# Sample Learning Program
# Created by Shaheer Ziya

import scipy as sp
import matplotlib.pyplot as plt
import numpy as np


# def f(r, t):
#   return None


def dx_by_dt(x, t):
  ''''Evaluates the derivative of x at time t.'''
  return -x**3 + np.sin(t)


def main():
  start, end = 0.0, 10.0
  STEPS = 10000
  h = (end - start) / STEPS # step size
  
  # Initial Condition [ x(0) = 0 ]
  x = 0.0

  tPoints = np.linspace(start, end, STEPS)
  xPoints = [x]

  print(tPoints)


  for t in tPoints[:-1]:
    x += h * dx_by_dt(x, t)
    xPoints.append(x)

  
  
  fig, axs = plt.subplots(1, 1)
  axs.plot(tPoints, xPoints)
  axs.set_xlabel('t')
  axs.set_ylabel('x')
  plt.show()





main()