
import numpy as np
x = np.linspace(-3, 3, 100)
print(x)
y = np.linspace(-3, 3, 100)
z = x*y*np.exp(-(x**2+y**2)/2)
print("z = ", z)
