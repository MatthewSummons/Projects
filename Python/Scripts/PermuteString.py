# Print all the permutations of a string

def permuteString(chars: str, ):
  if (len(chars) <= 1):
    return chars
  
  for i, specialChar in enumerate(chars):
      # Permuations of the string with the special character removed
      # Special character group (i)
      permuteString(chars[:i] + chars[i+1:]) + specialChar
      

  


def main():
  chars = "cat"
  permuteString(chars)
main()