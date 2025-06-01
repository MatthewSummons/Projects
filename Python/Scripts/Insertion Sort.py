# Script to visualize Insert Sort
# Shaheer Ziya

from time import time
from numpy import float128


class Array:
  def __init__(self, data:list) -> None:
    self.data = data
    self.length = len(array)

  def __str__(self) -> str:
    return str(self.data)

  def __repr__(self) -> str:
    return str(self.data)

  def __len__(self) -> int:
    return self.length

  def __getitem__(self, index:int) -> int:
    return self.data[index]
  
  def __setitem__(self, index:int, value:int) -> None:
    self.data[index] = value

def insertSort(array: Array, stepCount: int, completionTime: float128) -> tuple[Array, int, int]:
  for currentCardIndex in range(1, len(array)):
    currentCard = array[currentCardIndex]
      
    print(f"This is step {currentCardIndex}. The current card is {currentCard}")
    
    comparedCardIndex = currentCardIndex - 1
    while comparedCardIndex >= 0 and array[comparedCardIndex] > currentCard:
      
      stepCount += 1

      array[comparedCardIndex + 1] = array[comparedCardIndex]
      
      print(f"{str(array): ^50}")
      
      comparedCardIndex -= 1
   
    array[comparedCardIndex + 1] = currentCard

    print(f"{str(array): ^50}")
    print()
  
  return array, stepCount, completionTime

def main():
  array = [31, 41, 59, 26, 41, 58]
  sorted_array, stepCount = insertSort(array)[0]
  
  print("The sorted array is:")
  print(f"{str(sorted_array): ^50}")


main()