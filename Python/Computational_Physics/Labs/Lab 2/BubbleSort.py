# BubbleSort.py
# Implement Bubble Sort in Python
# Created by Shaheer Ziya

import numpy as np

def bubbleSort(arr):
    n = len(arr)
    print("Sorting Process of a List of Integers by Bubble Sort: ")
    print()
    
    # Traverse through all array elements
    for i in range(n-1):
        # Last i elements are already in place
        for j in range(0, n-i-1):
            # Traverse the array from 0 to n-i-1
            # Swap if the element found is greater than the next element
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
            print(arr)


# 10 numbers in the range [1, 100)
rndm = np.random.randint(1, 100, 10)
# Numpy Arrays are iterable & mutable. Work like python's inbuilt lists
bubbleSort(rndm)