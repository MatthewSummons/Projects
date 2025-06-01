### Get the greatest common divisor of 'a' and 'b'
def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

### Extended euclidean algorithm
def gcde(a, b):
    if a == 0:
        return b, 0, 1
    else:
        g, y, x = gcde(b % a, a)
        return g, x - (b // a) * y, y

"""
Get the modular inverse of 'b' under modulus 'a'
I.e., returns x that b * x mod a = 1
"""
def modinv(a, b):
    g, x, y = gcde(b, a)
    return x % a

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
Nx: the x-th modulus
Cx: the x-th encrypted message

return m: the plain text

Note: m*m*m should be smaller than N1 * N2 * N3
"""
def recover_msg(N1, N2, N3, C1, C2, C3):
    assert gcd(N1, N2) == gcd(N2, N3) == gcd(N1, N3) == 1, (
        "The moduli should be pairwise coprime, if not we can use the previous task " +
        "to find the common factor and decrypt the message")
    N = N1 * N2 * N3
    # Partial Products
    n1, n2, n3 = N // N1, N // N2, N // N3
    # Multiplicative Inverses
    y1, y2, y3 = modinv(N1, n1), modinv(N2, n2), modinv(N3, n3)
    # CRT (X = M^3)
    m = int(root3((C1 * n1 * y1 + C2 * n2 * y2 + C3 * n3 * y3) % N))
    return m

def get_student_number():
    return "3035946760"
    
