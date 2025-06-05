# Print Histogram to File
# Created by Shaheer Ziya

filePath = r"Lab 3/HKPop2020data.txt"

data = []
with open(filePath, 'r') as f:
  for lineNum, line in enumerate(f):
    # Skip the first 2 lines
    if lineNum in (0, 1): continue
    
    data += [line.split(" ")]

for line in data:
  line[1] = int(line[1])
  line[2] = int(line[2])

with open(r'HKPop2020hist.txt', 'w') as f:
  f.write("Mid-year Population in Hong Kong by Age Group and Sex for 2020\n")
  f.write("(in nearest ten thousands)\n")
  for line in data:
    f.write(f"{line[0]:^5} | {('#' * round(line[1]/1e4)) + ('&' * round(line[2]/1e4)):<65} ({round(line[1]/1e4)}/{round(line[2]/1e4)})\n")



