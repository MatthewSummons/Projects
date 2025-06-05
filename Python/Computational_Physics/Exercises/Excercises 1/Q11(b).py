# Q11(b).py
# Random Bulshit go
# Created by Shaheer Ziya on 21-02-2022 07:31 UTC

import numpy as np

def main():
    array = np.arange(1,17).reshape(4,4)
    print(array)
    #print(array[:, 2])
    #print(array[1::2,:3])
    print(array[::3,::2])

    #! arr1 = np.arange(1,13).reshape(4,3)
    #print(arr1[2,:])
    #print(arr1[:, 2])
    #print(arr1[1:3,-2:])
    #print(arr1[0::2,:])





main()