# 4(a).py
# Draw a histogram in matplotlib
# Created by Shaheer Ziya

import numpy as np
import matplotlib.pyplot as plt

# Take a 1000 samples from the normal distribution
numSamples = 1000
mean, SD = 100, 50
samples = np.random.normal(mean, SD, numSamples)

fig, ax = plt.subplots()
ax.hist(samples, bins = 50)

plt.show()