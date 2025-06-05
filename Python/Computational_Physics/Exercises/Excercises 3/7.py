import matplotlib.axes as ax
import numpy as np

fPath = r"Excercises 3/PYPL.txt"

def main():
  languages, share = np.genfromtxt(fPath, delimiter=" ", dtype=str, comments=None, usecols=(1,2,3,4,5,6,7))
  
  # print(language)
  # print(share)

  # From PyPlot
  # plt.pie(share, labels=languages, autopct='%1.1f%%', pctdistance=0.8)
  # plt.title("PopularitY of Programming Language (PYPL) Index 2020")

  fig, axs = plt.subplots()
  axs.Axes.pie(share, labels=languages, autopct='%1.1f%%', pctdistance=0.8)
  # plt.Axes.pie(share, labels=languages, autopct='%.1f%%', pctdistance=0.8)
  axs.title("PopularitY of Programming Language (PYPL) Index 2020")

  plt.show()

  


main()