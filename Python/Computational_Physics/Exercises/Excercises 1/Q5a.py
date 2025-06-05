# Q5a.py
# Takes in a +ive int and prints the sum of cubes less than it
# Created by Shaheer Ziya on 11-02-2022

def readPosInt():
    while True:
        try:
            n = int(input("Input a positive integer: "))
            if n <= 0: raise ValueError
            break
        except:
            print("Try again with a positive integer")
    return n


def calculateSumCubes(num):
    count = 0
    for i in range(num+1):
        count += (i**3)
    return count

def main():
    n = readPosInt()
    count = calculateSumCubes(n)
    print(count)

#main()             !!!!!!!!!!

#Can be improved with formula, no need to iterate


x= "abcdefghij"
print(x[::3])
print(x[-2::-2])
