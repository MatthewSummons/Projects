# Canada's Postal Codes
# Created by Shaheer Ziya

# Postal Code Letters & Their Corresponding Territories
postalCodes = {
    'A': "Newfoundland and Labrador",
    'B': "Nova Scotia",
    'C': "Prince Edward Island",
    'D': "Invalid",
    'E': "New Brunswick",
    'F': "Invalid",
    'G': "Quebec",
    'H': "Quebec",
    'I': "Invalid",
    'J': "Quebec",
    'K': "Ontario",
    'L': "Ontario",
    'M': "Ontario",
    'N': "Ontario",
    'O': "Invalid",
    'P': "Ontario",
    'Q': "Invalid",
    'R': "Manitoba",
    'S': "Saskatchewan",
    'T': "Alberta",
    'U': "Invalid",
    'V': "British Columbia",
    'W': "Invalid",
    'X': "Nunavut or Northwest Territories",
    'Y': "Yukon",
    'Z': "Invalid"
}

def main():
  # Obtain postal code from user
  code = input("Enter a Canadian postal code: ")
  # Obtain first letter of postal code
  firstLetter = code[0]
  # Obtain corresponding territory
  territory = postalCodes[firstLetter]

  # Determine if the area is rural or urban
  secondLetter = code[1]
  # 0 -> Rural, 1-9 -> Urban
  if secondLetter == '0':
    rurality = "a rural"
  elif (not secondLetter.isdigit()):
    rurality = "invalid"
  else:
    rurality = "an urban"

  # Error Handling & I/O
  # If the postal code's letter is invalid
  if territory == "Invalid":
    print("The first character in the postal code is invalid!")
    print("It cannot be D, F, I, O, Q, U, W, or Z!")
  # If the postal code's digit is invalid
  elif rurality == "invalid":
    print("The second character in the postal code must be a digit!")
  else:
    print(f"The postal code is for {rurality} address in {territory}.")

main()