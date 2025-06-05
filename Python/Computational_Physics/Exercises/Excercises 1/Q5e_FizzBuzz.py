# Fizz_Buzz.py
# Classic Fizz(3), Buzz(5), FizzBuzz(3*5)
# Created by Shaheer Ziya on 11-02-2022

for n in range(1,101):
    print(n, end=" ")
    if n % (3 * 5) == 0:
        print("FizzBuzz")
    elif n % 3 == 0:
        print("Fizz")
    elif n % 5 == 0:
        print("Buzz")
    else:
        print()