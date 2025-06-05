# Q10b.py
# Print coressponding lines of two files
# Created by Shaheer Ziya

with open("access.log") as f1, open("lines.txt") as f2:
    for line in zip(f2, f1):
        print(line[1] + line[0],end='')