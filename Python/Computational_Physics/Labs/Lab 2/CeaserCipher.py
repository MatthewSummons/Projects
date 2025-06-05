# CeaserCipher.py
# Implement A Ceaser Cipher
# Created by Shaheer Ziya

# Obtain the key and plain text

key = -1
while (key < 1) or (key > 25):
  key = int(input("Please provide a key in the interval [1,25]: "))
text = input("Enter the message you want encrypted: ")

encoded = ""

for char in text:
  if char.lower() < 'a' or char.lower() > 'z':
    encoded += char
  elif char.isupper():
    encoded += chr((ord(char) + key - 65) % 26 + 65)
  elif char.islower():
    encoded += chr((ord(char) + key - 97) % 26 + 97)


print(f"The encoded message is {encoded}")
print(f"The decoded message in {text}")