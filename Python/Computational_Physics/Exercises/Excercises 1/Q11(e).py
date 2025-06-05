# Q11(e).py
# Convert Celsius to Farenheit

import numpy as np

a = np.array(
    [[32],
    [56],
    [67],
    [99]]
)

def C_F(C):
    return (9/5 * C) + 32

converted = C_F(a)
print(converted)