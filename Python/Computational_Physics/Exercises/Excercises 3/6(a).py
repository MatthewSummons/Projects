# Plot Some Interesting Data
# Created by Shaheer Ziya on 29/03/2022

import numpy as np
import matplotlib.pylab as plt
# import os

filePath = r"Excercise 3/Q6_Data.txt"
# print(os.path.isfile(filePath))

data = np.genfromtxt(filePath, delimiter=",", skip_header=1, usecols=(1,2,3), comments="//")

langNames = np.genfromtxt(filePath, delimiter=",", skip_header=1, usecols=(0),dtype="U16", comments="//")

fig, axs = plt.subplots()
axs.scatter(data[:,0], data[:,2], s=data[:,1]/1e3, marker="o")

plt.show()

