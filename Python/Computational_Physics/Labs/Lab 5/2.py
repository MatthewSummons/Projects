# Data Fit
# Created by Shaheer Ziya

import matplotlib.pyplot as plt
import numpy as np

P = 4 * np.pi * 10e-5

# Radii & Intensity
r = np.array([1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4])
I = np.array([0.987,0.662,0.525,0.373,0.308,0.262,0.191,0.184]) * 10e-5

def main():
  A = np.vstack((np.log(r), np.ones(len(r)))).T
  B = np.log(I)

  x, resid = np.linalg.lstsq(A, B)[0:2]
  m, k = x

  print(m, k, resid)


  plt.subplots()

  # plt.plot(np.log(r), np.log(I), 'o', label='Data')
  plt.plot(np.log(r), np.log(P / (4 * np.pi * r**2)), 'o', label='Theory')
  
  plt.plot(np.log(r), m*np.log(r) + k, 'r', label='Fit')

  plt.title("Sound Intensity from a Point Source")
  plt.xlabel("Log(r) (m)")
  plt.ylabel("Log(I) $(W/m^{-2})$")

  plt.show()

main()