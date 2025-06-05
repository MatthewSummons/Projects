# Recurrence Relation Fucntion Approximation
# Created by Shaheer Ziya

# import math

def fsum(x, n):
  '''Aprroximate the fucntion x/(1+x)^2 using a recurrence relation'''
  # Base Case
  if n == 1:
    return 1 * x
  # Recursive Call
  else:
    return (n * pow(x, n)) + fsum(x, n-1)


def main():
  for x in [0.1, 0.2, 0.3, 0.4]:
    for n in [2, 5, 10, 50, 100]:
      print(f"The function x/(1-x)^2 evaluated at {x} with {n} steps is {fsum(x, n):.8f}")
    print()

main()