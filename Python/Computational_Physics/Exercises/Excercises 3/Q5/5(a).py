# 5(a).py
# Plot the demographics of 5 US Cities over time
# Created by Shaheer Ziya

from operator import imod
import os
import numpy as np
import matplotlib.pyplot as plt

BASE = r"Excercise 3/Q5/us-city-populations"
filepaths = [os.path.join(BASE, name) for name in os.listdir(BASE)]
all_files = []

for path in filepaths:
  with open(path, 'r') as city_pops:
    file = city_pops.readlines()
    all_files.append(file)


fig, ax = plt.subplots()
plt.title("US Population Over The Years")

line_styles = ["solid", "dashed", "dashdot", "dotted", "solid"]

for idx, file in enumerate(all_files):
  data = np.genfromtxt(file, dtype=int)
  years, population = data[:, 0], data[:, 1]

  ax.plot(years, population, ls=line_styles[idx])


# Dsiplay City Names on the plot
cityNames = os.listdir(BASE)
cityNames = [cityName[:-4].replace("_", " ").title() for cityName in cityNames]
plt.legend(cityNames, loc="lower left")

plt.show()
  
