# Q9c.py
# Return the minimum of four nums

def main():
    nums = input("Enter comma separted numbers: ").replace(" ","").split(",")
    min = 10000
    for i in nums:
        if int(i) < min: min = int(i)
    print(min)
        
main()