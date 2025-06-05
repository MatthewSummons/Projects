# Q5(b).py
# Model Temperature/Intensity
# Created by Shaheer Ziya

from turtle import color
import matplotlib.pyplot as plt
import numpy as np
Polynomial = np.polynomial.Polynomial


Data = np.array([
#  T/K ; I±𝜎 W/m^2
  (300, 431.0, 45.6),
  (350, 780.2, 85.0),
  (400, 1415.4, 77.4),
  (450, 2327.3, 153.3),
  (500, 3225.0, 310.8),
  (550, 5400.9, 464.4),
  (600, 7295.5, 379.8)
])

T, I, sigma = Data[:, 0], Data[:, 1], Data[:, 2]
Degree = 4
Weights = (1 / Data[:, 2] ** 2)
Fit = Polynomial.fit(T, I, Degree, w=Weights)


fig, ax = plt.subplots()
x = np.linspace(250, 650, 100000)
ax.plot(x, Fit(x), label="Fit")
ax.errorbar(T, I, sigma, marker="o", color='r', capsize=3, label="Data")

plt.title("Intensity Against Temperature")
ax.set_xlabel("Temperature / K")
ax.set_ylabel("Intensity / $Wm^{-2}$")
ax.grid()
ax.legend()


plt.show()