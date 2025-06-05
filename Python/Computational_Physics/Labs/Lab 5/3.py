# Plot Damped Oscillator
# Created by Shaheer Ziya

import matplotlib.pyplot as plt 
import numpy as np

wOverw0 = np.linspace(0, 2, 1000)
xiS = np.array([0.1, 0.2, 0.4, 0.6, 0.8])

def main():
  
  plt.subplots()
  
  for xi in xiS:
    M = 1 / (np.sqrt(
      ((1 - wOverw0**2)**2) + (4 * xi**2 * wOverw0**2)
    ))
    phi = np.arctan2(2*xi*wOverw0, 1 - wOverw0**2)

    plt.plot(phi, M, label=f"{xi}")
    
  plt.legend()
  plt.xlabel("Phase Lag $\phi$")
  plt.ylabel("Maginification ratio")
  plt.title("Forced Vibration with Damping")
  
  plt.show()


main()