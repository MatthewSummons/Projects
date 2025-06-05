# Read data from a text file using numpy
from struct import unpack
import numpy as np
import os

fName = os.path.join(r"Excercises 3/", r"mydata.txt")
# os.path.isfile(fName)

def main():
  data = np.genfromtxt(fName, delimiter="-", unpack=True)
  for col in data:
    print(col)

main()
