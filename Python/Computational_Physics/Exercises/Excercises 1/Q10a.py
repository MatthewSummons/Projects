# Q10a.py
# Print the number of lines in a file
# Created by Shaheer Ziya

from fileinput import filelineno


def main():
    with open('Access.log') as file:
        line_count = 0
        for line in file:
            line_count += 1
        print(f"This file has {line_count} lines.")

main()