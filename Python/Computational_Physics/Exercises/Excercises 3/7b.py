import matplotlib.pyplot as plt
import numpy as np

fPath = r'Excercises 3/Q8_Data.txt'

def main():
  jobTitles = np.genfromtxt(
      fPath, delimiter="-", dtype=str, unpack=True, skip_footer=1)
  salaries = np.genfromtxt(
      fPath, delimiter="-", dtype=int, unpack=True, skip_header=2)
  # print(jobTitles)
  # print(salaries)

  y_pos = np.arange(len(jobTitles))

  fig, axs = plt.subplots()

  axs.barh(y_pos, salaries, align='center', alpha=0.5)
  axs.set_yticks(y_pos, jobTitles)
  plt.show()

main()