# The Definition For the Queue Class
# Created by Shaheer Ziya

# A class to represent a queue using FIFO (First In First Out)
class Queue:

  def __init__(self, max_size: int = 10) -> None:
      self.queue = []
      self.max_size = max_size


  def __str__(self) -> str:
    elementsString = ""
    for idx, element in enumerate(self.queue):
      elementsString += f"Element {idx+1} in the queue = {element}\n"
    
    # Don't return last newline
    return elementsString[:-1]
  

  def enqueue(self, item) -> None:
      ''' Enter an integer to a queue '''
      # Ensure only integer values are added to the queue
      if type(item) != int:
        print("You can only add integers to the queue!")
      # See if the queue is full, if it is not then add to the queue
      elif len(self.queue) < self.max_size:
          self.queue.append(item)
      else:
          print("The queue is full and no new element can be added!")

    
  def dequeue(self):
    ''' Remove an integer from the queue and return the removed integr '''
    # See if the queue is empty, if it is not then remove from the queue
    if len(self.queue) > 0:
        # Return the removed element
        return self.queue.pop(0)

    else:
        print("The queue is empty and there is no front element!")
      
  def isempty(self) -> bool:
    ''' Check if the queue is empty '''
    if len(self.queue) == 0:
        return True
    else:
        return False
  
  def isfull(self) -> bool:
    ''' Check if the queue is full '''
    if len(self.queue) == self.max_size:
        return True
    else:
        return False
  
  def count(self) -> int:
    ''' Return the number of items in the queue '''
    
    print(f"Number of elements in the queue = {len(self.queue)}")
    return len(self.queue)
  







def main():
  a = Queue(3)
  
  print(a.isempty())
  
  a.enqueue(1.2)

  a.enqueue(1)
  a.enqueue(2)
  a.enqueue(3)
  a.enqueue(4)

  print(a.isfull())

  a.count()

  print(a.dequeue())

  print(a)

  print(a.dequeue())
  print(a.dequeue())
  print(a.dequeue())



main()