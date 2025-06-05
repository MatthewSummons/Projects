# 3D Surface & Wirefram Plots
# Created by Shaheer Ziya

import matplotlib.pyplot as plt
import matplotlib.cm as cm
import numpy as np

# Constant
L, steps = 2, 500
# Parameters for Wave Fucntion
m, n = 4, 3

def psi(x, y):
  return (2/L) * np.sin(m * np.pi * (x/L)) * np.sin(n * np.pi * (y/L))


def main():
  x = np.linspace(0, L, steps)
  y = x.copy()

  X, Y = np.meshgrid(x, y)
  Z = psi(X, Y)
  
  fig, axs = plt.subplots(1, 2, subplot_kw={"projection": "3d"})

  axs[0].plot_wireframe(X, Y, Z, rstride=15, cstride=15)
  axs[0].set_title("Wireframe Plot")
  axs[0].set_xlabel("x")
  axs[0].set_ylabel("y")
  axs[1].plot_surface(X, Y, Z, rstride=30, cstride=30)
  axs[1].set_title("Surface Plot")
  axs[1].set_xlabel("x")
  axs[1].set_ylabel("y")

  fig.suptitle("Wave Function for a 2D Infinite Square Well")

  plt.show()


main()