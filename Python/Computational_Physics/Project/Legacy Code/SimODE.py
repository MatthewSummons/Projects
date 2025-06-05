##
    # Program Works using odeint 
##

import matplotlib.pyplot as plt
import numpy as np
import scipy.integrate as sp

def dr_by_dt(r, t):
  x, y = r
  return np.array([(x*y - x),(y - x*y + (np.sin(t))**2)])

def main():
  t = np.linspace(0, 10, 100)
  # x(0) = y(0) = 1
  r0 = np.array([1, 1])

  sol = sp.odeint(dr_by_dt, r0, t)

  fig, axs = plt.subplots(1, 1)
  axs.plot(t, sol[:, 0], label = '$x(t)$')
  axs.plot(t, sol[:, 1], label = '$y(t)$')

  plt.legend(loc='best')
  plt.grid()
  plt.show()


main()