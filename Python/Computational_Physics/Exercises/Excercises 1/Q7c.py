# Q7c.py
# Takes in a comma separated sequence of numbers & returns the squares of all odd-ints in it
# Created by Shaheer Ziya on 12-02-2022


def tuple_pattern():
    
    #white-space, then convert to a list contanint str-nums
    seq = input("Enter a sequence of comma-separated integers: ").replace(" ", "").split(",")
    seq_fin = [int(i)**2 for i in seq if int(i) % 2 != 0]

    print(seq_fin)


def list_pattern(n):
    """
    Output a list of the pattern [[n], [n-1, n], ... [1, 2, ... , n-1, n]]
    """
    final_list = []
    buffer_list = []
    for i in range(n,0,-1):
        buffer_list = buffer_list + [i]
        final_list.append(buffer_list) 
    print(final_list)
    
    # Via list comprehensions
    print( [ [i for i in range(n, j-1, -1)] for j in range(n, 0, -1) ] )


def main():
    list_pattern(int(input("Enter a whole number: ")))

main()

