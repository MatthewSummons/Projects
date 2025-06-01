### Get cube root of 'x'
def root3(x):
    h = 1
    while h ** 3 <= x:
        h *= 2
    l = h // 2
    m = 0
    while l < h:
        m = (l + h) // 2
        if m ** 3 < x and l < m:
            l = m
        elif m ** 3 > x and h > m:
            h = m
        else:
            return m
    return m + 1


"""
N: the modulus
E: the public encryption exponent (in this task, E always equals to 3)
C: the cipher text

return m: the plain text
"""
def recover_message(N, E, C):
    m = 0
    for k in range(-10, 11):
        m = root3(C + k * N)
        if pow(m, E, N) == C:
            return m
    return 0

def get_student_number():
    return "3035946760"