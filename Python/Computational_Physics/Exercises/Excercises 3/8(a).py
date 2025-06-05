import matplotlib.pyplot as plt
import numpy as np

def main():
  # x(t) = A sin(wt + phi); v(t) = Aw cos(wt + phi)
  A, w, phi = 1, 10 * np.pi, np.pi/6 

  t = np.linspace(0, 2*np.pi, 1000)

  # x(t) = A sin(wt + phi); v(t) = Aw cos(wt + phi)
  x = A * np.sin(w*t + phi)
  v = A * w * np.cos(w*t + phi)

  fig, axs = plt.subplots(2, 1)
  fig.subplots_adjust(hspace=0)


  axs[0].plot(t, x, label='x(t)') 
  axs[0].set_xticklabels("")
  axs[1].plot(t, v, label='x(t)') 
  axs[0].grid()
  axs[0].tick_params(direction="in")

  plt.show()

main()