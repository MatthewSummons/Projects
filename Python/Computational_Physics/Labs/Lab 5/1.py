# Chebyshev Polynomials
# Created by Shaheer Ziya

import numpy as np

def main():
  
  coeff_table = tuple()
  
  chebyshev_coefficents = [1]
  for i in range(10):
    coeff_table += list(np.polynomial.Chebyshev.convert(np.polynomial.Chebyshev(coef=chebyshev_coefficents),
      kind = np.polynomial.Polynomial).coef),
    chebyshev_coefficents.insert(0, 0)
  
  
  # Print the polynomial
  for i, pol in enumerate(coeff_table):
    polynomial_str = ""
    for idx, coeff in enumerate(pol[::-1]):
      # Determine the sign of the coefficient
      if coeff > 0: sign = " + "
      else: sign = " - "
      
      # Determine the power of the coefficient
      power = len(pol) - idx - 1

      # Determine the string representation of the coefficient
      if (coeff == 1 and power != 0): coeff_str = ""
      else: coeff_str = str(abs(int(coeff)))
      

      if coeff == 0:
        continue
      elif power == 0:
        polynomial_str += sign + coeff_str
      elif power == 1:
        polynomial_str += sign + coeff_str + "x"
      else:
        polynomial_str += sign + coeff_str + "x^" + str(power)
    
    print(f"T_{i}(x) = " + polynomial_str[3:])

main()
