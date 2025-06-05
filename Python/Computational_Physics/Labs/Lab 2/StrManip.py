# StrManip.py
# Perform String Manipulation based on user choices
# Created by Shaheer Ziya on 23-03-2022

def main():

  # Ask for the line to perform string manip on
  line = input("Enter a line of text without punctuation mark: \n")

  # Ask for type of string manipulation (Provide 3 choices)
  print("Type a number 1 to 3 to do one of the following operations")
  print(" 1 - print the line of text with all its blank spaces removed")
  print(" 2 - print the line of text with each word in reversed order")
  print(" 3 - print the number of words in the line of text")

  choice = input("Enter your choice (1-3): ")

  # Validfy input
  if choice not in ('1', '2', '3'):
    print("Invalid input. You must type 1 or 2 or 3!")
  
  elif choice == '1':
    print("The line of text with all its blank spaces removed:")
    print(line.replace(" ", ""))    # Replace whitespace with nothing
  
  elif choice == '2':
    word = ""
    # Iterate over all the character in line
    for chr in line:
      # Identify the words in the line
      if chr != " ":
        word += chr
      # Reverse the identified word (Words are separated by whitespaces)
      else:
        line = line.replace(word, word[::-1])
        word = ""
    
    print("The line of text with each word in reversed order:")
    print(line)
  
  elif choice == '3':
    wordCount = 0
    word = ""
    # Separate words in the line & count them
    for chr in line.replace("  ", " "):
      if chr != " ":
        word += chr
      else:
        wordCount += 1
        word = ""
    
    print(f"The number of words in the line of text: {wordCount}")
  


main()
