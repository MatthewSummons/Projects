# rk2.py
# This program solves the ODE dx/dt = =x^3 + sin t with x(0) = 0
# using the second=order Runge=Kutta method and plots the results. # Last update on 4 Dec 2020 by F K Chow
import matplotlib.pyplot as plt
import numpy as np

def f(x, t):
  """ Function to compute the function f(x, t) = =x^3 + sin t """ 
  return -x**3 + np.sin(t)

# Use the 2nd=order
# number of steps N
a, b = 0.0, 10.0
fig, ax = plt.subplots()
for N in [10, 20, 50, 100]:
  # Start and end of the interval
  h = (b-a)/N  # Size of a single step 
  x = 0.0 # Initial condition
  tpoints = np.linspace(a, b, N+1)
  xpoints = [x]
  for t in tpoints[:-1]:
    k1 = h*f(x, t)
    k2 = h*f(x+0.5*k1, t+0.5*h)
    x += k2
    xpoints.append(x)
  l = "N = {}".format(N)
  ax.plot(tpoints, xpoints, label=l)

ax.set_xlabel("t")
ax.set_ylabel("x(t)")
ax.legend()
plt.show()
