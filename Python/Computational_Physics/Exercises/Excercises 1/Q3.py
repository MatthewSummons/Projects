# Q3.py
# Compute exponents of complex numbers and represent them in polar co-ordinates
# Created by Shaheer Ziya on 11-02-2022

import cmath

print("Please input complex numbers in the form x +yj !")
cNum = complex(input("Input the comlex number you want to raise to the power e: "))

print(f"e^{cNum} = {cmath.exp(cNum):.3f}")
print(f"The polar form of {cNum} is {cmath.polar(cNum)}")