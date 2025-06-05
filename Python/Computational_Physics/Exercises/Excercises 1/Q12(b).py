# Q12(b).py
# Count letters in input
# Shaheer Ziya

Text = input("Please input a string: ").replace(" ","").lower()

count_char = {}

for char in Text:
    if char in count_char:
        count_char[char] += 1
    else:
        count_char[char] = 1
print(count_char)
