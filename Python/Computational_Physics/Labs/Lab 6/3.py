# Series LRC Circuit
# Created by Shaheer Ziya

import matplotlib.pyplot as plt
from scipy.integrate import odeint
import numpy as np


# L = 0.5 H, R = 20 ohms,C = 0.001 F 
L, R, C = 0.5, 20, 0.001

def emf(t):
  return 100 * np.sin(60 * t)

def dr_dt(r, t: float) -> float:
  q, I = r
  dq_dt = I
  dI_dt = 1/L * (emf(t) - (R * I) - (q/C))
  return np.array([dq_dt, dI_dt])


def main():
  # r = q, I
  # Interval to solve for and plot in
  start, end = 0, 5
  # Number of partitions of the interval
  STEPS = 1000
  # The array holding the partitions
  t = np.linspace(start, end, STEPS)

  # Initial conditions
  r0 = 0, 6
  
  # Solve the differential equation
  sol = odeint(dr_dt, r0, t)

  ### Plotting th Solution ###
  fig, axs = plt.subplots(2)
  fig.suptitle('Series LRC Circuit')
  fig.subplots_adjust(hspace=0)
  axs[0].plot(t, sol[:,0], color='blue', label='Charge q(t)')
  axs[1].plot(t, sol[:, 1], color='orange', label='Current I(t)')

  # Visual Aspects
  for i in (0,1):
    if i == 0:
      axs[i].set_xticklabels("")
    axs[i].set_xlim(start, end)
    axs[i].set_xlabel('Time (s)')
    axs[i].tick_params(direction="in")
    axs[i].legend()


  ### Final Tests ###

  fig, axs = plt.subplots()
  axs.plot(sol[:,0], sol[:,1])


  plt.show()

main()
