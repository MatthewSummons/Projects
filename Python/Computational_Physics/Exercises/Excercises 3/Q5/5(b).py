# Temperature Intenstiy Plotting
# Created by Shaheer Ziya on 29/03/2022

import numpy as np
import matplotlib.pylab as plt

def main():
  # Temperatures in [300,600] with step of 50K
  Temp = np.arange(300, 650, 50)
  
  # Intensity of the light in W/m^2
  Intensity = np.array([431, 780.2, 1415.4, 2327.3, 3225, 5400.9, 7295.5])
  Errors = np.array([45.6, 85, 77.4, 153.3, 310.8, 464.8, 379.8])

  errorFit = np.polynomial.Polynomial.fit(Temp, Intensity, deg=4, w=1/Errors)

  # Plotting
  figure, axes = plt.subplots()
  axes.errorbar(Temp, Intensity, yerr=Errors, fmt='o')
  axes.plot(Temp, errorFit(Temp), 'r-')

  axes.set_title("Temperature Intensity Plot")
  axes.set_xlabel("Temperature (K)")
  axes.set_ylabel("Intensity (W/m$^2$)")
  plt.grid(True)

  plt.show()



main()