# Q5d.py
# Detect palindromes from inputs
# Created by Shaheer Ziya on 11-02-2002 (dd-mm-yy)

In = input("Input the word to check for palindrom-iness: ")

if In == In[::-1]:
    print(f"{In} is a palindrome")
else:
    print("It's not a palindrome")