# Convert Floating Point to Monetary Values
# Created by Shaheer Ziya

# A class to convert floating point values to monetary values
class MoneyFmt:

  def __init__(self, value: "float" = 0) -> None:
    '''Initialize the MoneyFmt object with the passed value argument'''
    self.value = value
  

  def __str__(self) -> str:
      '''Return a human readable string representation of the MoneyFmt object'''
      # Add commas & round to 2 decimal places
      return f"${self.value:,.2f}"
  

  def __repr__(self) -> str:
      '''Return a string representation of the MoneyFmt object that instantiates the object under eval()'''
      return f"MoneyFmt({self.value})"
  

  def update(self, value: "float" = None) -> None:
      '''Update the existing value of the MoneyFmt object'''
      self.value = value

  
  def isnonzero(self):
      '''Return True if the magnitude of value of the MoneyFmt object is not less than 0.005'''
      if abs(self.value) >= 0.005:
        return True
      else:
        return False


def main():
  cash = MoneyFmt(135.79)
  
  print(repr(cash))
  print(str(cash))
  
  cash.update(5000000.2468)
  
  print(repr(cash))
  print(str(cash))

  cash.update(-1386.42)
  print(str(cash))

  print(cash.isnonzero())

  cash.update(-0.0049)
  print(cash.isnonzero())

main()