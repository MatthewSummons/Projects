# 4(b).py
# Find the probability of having C13- in a C60 molecule
# Created by Shaheer Ziya

from traceback import print_tb
import matplotlib.pyplot as plt
import numpy as np

def nCk(n: "int",k: "int") -> int:
  if k == 0 or n == k:
    return 1
  else:
    return nCk(n-1, k-1) + nCk(n-1, k) 

# B(n, p): n = number of bernoulli trials, p = probability of hitting a C13 atom
n, p = 60, 0.017

numTrials = 100000
samples = np.random.binomial(n, p, numTrials)

hits = samples[samples <= 4]

sampled_probability = len(hits) / numTrials

theoritcal_probability = 0
for n in range(5):
  theoritcal_probability += (nCk(60, n) * (p ** n) * ((1-p)**(60-n)))

### Figure to see samples ###
# fig, ax = plt.subplots()
# ax.hist(hits)
# plt.show()


print(f"The probability that there are upto 4 C-13 atoms in the buckyball is {sampled_probability:.5f}")
print(f"The theoretical probability is {theoritcal_probability: .5f}")
