# Q6a.py
# Removes certain chars from a string
# Created by Shaheer Ziya on 11-02-2022

rm_chr = 'b'

string = input(f"Enter a string from which all {rm_chr}'s will be removed: ")

removal = tuple(string)
temp_tupl = tuple()

for char in removal:
    if char != rm_chr:
        temp_tupl += (char),

new_str = "".join(temp_tupl)
print(new_str)
    