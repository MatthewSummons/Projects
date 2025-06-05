# 2.py
# Data Analysis
# Created by Shaheer Ziya

filePath = r"Lab 3/HKFM.txt"


maleYrData, femaleYrData = {}, {}
Ages = []

def YearMode(data):
  '''Find the mode of the number of marriages in a year and return its corresponding Age Group'''
  # Find the index of the mode value and match it with it's corresponding Age Group
  return Ages[data.index(max(data))]

# Init dicts with years as keys
years = ["1995", "2000", "2005", "2010", "2015", "2020"]
for year in years:
  maleYrData[year] = []
  femaleYrData[year] = []


with open(filePath, 'r') as f:
  for (lineNum, line) in enumerate(f):
    # Skip header lines
    if lineNum in (0, 1, 2): continue
    # Male Data
    elif lineNum < 11:
      # Separate words in line
      lineList = line.split("	")
      # Obtain Age Groups (Need only be done once)
      Ages += lineList[0],
      
      # Obtain data for each of the years
      for idx, year in enumerate(years):
        maleYrData[year] += int(lineList[idx+1]),
    
    # Go over the data for the females
    elif lineNum > 11:
      lineList = line.split("	")
      # Obtain data for each of the years
      for idx, year in enumerate(years):
        femaleYrData[year] += int(lineList[idx+1]),
  

# Print the organized data
print(f"{'Sex':^10} {'Year':^10} {'Total Number':^15} {'Dominant Age Group':^15}")
print("-"*60)

# Print the statistics for the males
for year in years:
  print(f"{'Male':^10} {year:^10} {sum(maleYrData[year]):^15} {YearMode(maleYrData[year]):^15}")

# Print the statistics for the females
for year in years:
  print(f"{'Female':^10} {year:^10} {sum(femaleYrData[year]):^15} {YearMode(femaleYrData[year]):^15}")