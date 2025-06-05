# Data Analysis
# Created by Shaheer Ziya

filePath = r"Lab 3/HKFM.txt"

def main():
  # Extract data from the text file
  dt = np.dtype( [("1995", int), ("2000", int), ("2005", int), ("2010", int), ("2015", int), ("2020", int)] )
  maleData = np.genfromtxt(filePath, dtype=dt, delimiter="	", skip_header=3, skip_footer=9, usecols=(1,2,3,4,5,6))
  femaleData = np.genfromtxt(filePath, dtype=dt, delimiter="	", skip_header=12, usecols=(1,2,3,4,5,6))

  # maleData.T
  # femaleData.T

  print(maleData)

  # Print the organized data
  print(f"{'Sex':^10} {'Year':^10} {'Total Number':^15} {'Dominant Age Group':^20}")
  print("-"*72)

  years = ["1995", "2000", "2005", "2010", "2015", "2020"]
  for year in years:
    print(maleData.find(max(maleData[year])))
    # print(f"{'Male':^10} {year:^10} {sum(maleData[year]):^15} {'P':^20}")


main()