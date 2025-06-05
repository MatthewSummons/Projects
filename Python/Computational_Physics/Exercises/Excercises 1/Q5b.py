# Q5b.py
# Print the multiplication table upto nxn for n<=10
# Created by Shaheer Ziya on 11-02-2022

n = int(input("Please enter an whole number: "))
upto = n+1

for i in range(1,upto):
    print(f"{i:^3} x {n:^3} = {i*n:>5}")