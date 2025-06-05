# 2(b).py
# Model  fitting the Volume Pressure of a peculiar gas
# Created by Shaheer Ziya on 17/3/2022 UTC+08 16:00

import matplotlib.pyplot as plt
import numpy as np
Polynomial = np.polynomial.Polynomial


# Volume in litres, Pressure in standard atmospheres
data = np.array((
  #  V      P
  (0.608, 0.05),
  (0.430, 0.10),
  (0.304, 0.20),
  (0.215, 0.40),
  (0.192, 0.50)
))

# P (Independent Variable), 1/V (Dependent Variable)
Fit = Polynomial.fit(data[:,1], 1/data[:,0], deg=3)

# Generate the graph
fig, ax = plt.subplots()
x = np.linspace(0, 0.6, 1000)
ax.plot(x, Fit(x))
# P and 1/V
ax.plot(data[:, 1], 1/data[:, 0])

ax.set_xlabel("Pressure (atm)")
ax.set_ylabel("Volume (dm$^{-3}$)")

plt.show()
