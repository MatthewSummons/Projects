# Q9e.py
# Recursively calculate the sum of squares of ints upto n
# Created by Shaheer Ziya on 12-02-2022

from ast import Pass


def sum_of_squares(n):
    #Base Case
    if n == 0:
        # 0^2 = 0
        return 0
    #Conditional Recursive Call
    else:
        return pow(n, 2) + sum_of_squares(n-1)


def main():
    n = int(input("Enter a +ive integer: "))
    count = sum_of_squares(n)
    print(f"The sum of squares upto {n} is {count}")


main()
