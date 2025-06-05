# Legendre Polynomial
# Created by Shaheer Ziya

import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import curve_fit
from scipy.special import eval_legendre

x_data = np.linspace(-1, 1, 11)
y_data = np.array([0.91695, -0.19706, -0.29293, -0.04645,
             0.24494, 0.44410, 0.31141, -0.04369, -0.42651, -0.39541, 1.14994])

def try_fit(x, a, b, c, d, e):
  '''Degree 4 polynomial'''
  return a * (x ** 4) + b * (x ** 3) + c * (x ** 2) + d * x + e

def main():
  fitted_paramters = curve_fit(try_fit, x_data, y_data)[0]
  print(fitted_paramters)

  fig, axs = plt.subplots()

  
  X = np.linspace(-1, 1, 1000)
  
  fitted_curve = try_fit(X, *fitted_paramters)
  # axs.plot(x_data, y_data, 'o', label='Data')
  axs.plot(X, fitted_curve, '-', label='Fitted Curve')
  
  true_curve = eval_legendre(4, X)
  axs.plot(X, true_curve, '-', label='True Curve')

  plt.title("Legendre Polynomial $P_4(x)$")
  plt.xlabel("x")
  plt.ylabel("$y$")

  plt.legend()
  plt.show()



main()

