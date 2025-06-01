# All permutations of a string using recursion

def perm(A:list, k:int, n:int):
  if k == 0:
    return print(A)
  
  for i in range(k+1):
    swap(A, i, k)
    perm(A, k-1, n)
    swap(A, i, k)


def swap(A:list, i:int, j:int):
  temp = A[i]
  A[i] = A[j]
  A[j] = temp
  

def main():
  finalChar = 'c'
  alph = [chr(i) for i in range(ord('a'), ord(finalChar)+1)]
  n = len(alph) - 1
  
  perm(alph, n, n)

if __name__ == '__main__':
  main()
