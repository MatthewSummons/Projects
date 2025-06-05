# Histogram.py
# Print a histogram for some data in a given format
# Created by Shaheer Ziya

# Hard Code the tabular data
stdPopData = {
    "Central & Western" : 7, 
    "Wan Chai": 25, 
    "Eastern" : 22,
    "Southern" : 8,
    "Yau Tsim Mong" : 14,
    "Sham Shui Po" : 22,
    "Kowloon City" : 29,
    "Wong Tai Sin" : 6,
    "Kwun Tong" : 15,
    "Sai Kung" : 7,
    "Sha Tin" : 8,
    "Tai Po": 8,
    "North" : 8,
    "Yuen Long" : 27,
    "Tuen Mun" : 7,
    "Tsuen Wan" : 18,
    "Kwai Tsing" : 0,
    "Islands" : 0
}

# Print the histogram using f-strings
for area, pop in stdPopData.items():
  print(f"{area:>20} | {'*'*pop} ({pop})")
