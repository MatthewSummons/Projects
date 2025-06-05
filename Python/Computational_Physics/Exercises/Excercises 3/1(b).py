# Perform statistics on sample data

import numpy as np
import os

# Get to the file
BASE_NAME = r"Excercises 3"
fName = os.path.join(BASE_NAME, r"eg6-a-student-data.txt")
#! Debug line to check file path is correct
# print(os.path.isfile(fName))

def main():
  #! Part (i)
  # Read only the gender, height and weight of the candidates
  dt = np.dtype([("Gender","S1"), ("Height","f8"), ("Weight","f8")])
  data = np.genfromtxt(fName, usecols=(1, 3, 4), dtype=dt, comments="#", skip_header=7)

  #! Part (ii)
  heightData = data[np.isfinite(data["Height"])]
  weightData = data[np.isfinite(data["Weight"])]
  # Male specific data
  maleHeights = heightData["Height"][heightData["Gender"] == b'M']
  maleWeights = weightData["Weight"][weightData["Gender"] == b'M']
  # Female specific data
  femaleHeights = heightData["Height"][heightData["Gender"] == b'F']
  femaleWeights = weightData["Weight"][weightData["Gender"] == b'F']

  # Compute average height & weights
  muMaleHeight = np.mean(maleHeights)
  muMaleWeight = np.mean(maleWeights)
  muFemaleHeight = np.mean(femaleHeights)
  muFemaleWeight = np.mean(femaleWeights)

  # Output averages to screen
  print(f"The average male height in this sample is {muMaleHeight:.2f}")
  print(f"The average male weight in this sample is {muMaleWeight:.2f}")
  print(f"The average female height in this sample is {muFemaleHeight:.2f}")
  print(f"The average female height in this sample is {muFemaleWeight:.2f}")

  #! Part (iii)
  data = np.genfromtxt(fName, usecols = (0,3), dtype=[("ID", "U8"), ("Height", "f8")], missing_values="-", comments="#", skip_header=7)
  data.sort(order="Height")
  for std in data:
    print(f"{std[0]: ^5s} : {std[1]:.2f}")

main()

