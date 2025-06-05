# Twin Bar Chart
# Created by Shaheer Ziya

import matplotlib.pyplot as plt
import numpy as np

years = [2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020]
numEmployees = [277.0, 290.1, 309.0, 309.7, 316.7, 328.4, 342.0, 351.6, 337.5, 310.0]
shareEmployees = [7.75, 7.93, 8.30, 8.27, 8.39, 8.67, 8.95, 9.09, 8.77, 8.47]


def main():
  fig, ax1 = plt.subplots()
  ax1.bar(years, numEmployees, label = 'Number of Employees')
  ax1.set_ylabel('Number of Employees in Thousands')

  ax2 = ax1.twinx()
  ax2.plot(years, shareEmployees, label = 'Share of Employees', color = 'r')
  ax2.set_ylabel('Share of Employees')
  ax2.set_yticklabels(['{}%'.format(x) for x in shareEmployees])


  ax1.legend(loc='upper left')
  ax2.legend(loc='lower right')
  plt.title('Employees in Hong Kong\'s Construction Industry')
  plt.show()


main()