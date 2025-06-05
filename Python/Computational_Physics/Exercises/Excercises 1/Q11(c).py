# Q11(c).py
# Sort a 2x2 array across its axes
# Created by Shaheer Ziya on UTC+08 22:13

import numpy as np

array = np.array(
    [[13,2],
    [3,4]]
)

sorter = np.sort(array, axis = 0)
sorter2 = np.sort(array, axis= -1)
sorter3 = np.sort(array.flatten(), axis = 0)

print(sorter)
print(sorter2)
print(sorter3)