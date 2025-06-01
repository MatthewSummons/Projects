# Program to recursively reverse an array

Array = []

def reverseArray(Arr:list, i:int, j:int):
  # Base cases (For when empty or single element array)
  if i >= j: return
  # Swap the exterior elements
  Arr[i], Arr[j] = Arr[j], Arr[i]
  # Swap the middle elements
  reverseArray(Arr, i+1, j-1)



def main():
  x = [1,2,3,4,5,6,7,8,9,10]
  print("Original Array: ", x)
  
  reverseArray(x, 0, len(x)-1)
  print("Reversed Array: ", x)

main()