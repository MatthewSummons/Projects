# Q5c.py
# Prints a tuple containing all factors of n (inclusive)
# Created by Shaheer Ziya


n = int(input("Please enter an +ive integer: "))
factors = tuple()

for i in range(1, n+1):
    if n % i == 0:
        factors += (i),

print(factors)