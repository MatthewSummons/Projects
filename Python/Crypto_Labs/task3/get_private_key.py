### Get the greatest common divisor of 'a' and 'b'
def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

# Extended euclidean algorithm
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

"""
N1: your modulus
N2: your classmate's modulus

return result: True if n1 and n2 share the same prime number; otherwise, returns False
"""
def is_waldo(n1, n2):
    # If n1 and n2 share the same prime number, return True
    if gcd(n1, n2) != 1 and n1 != n2:
        return True
    return False

"""
This function will be called if is_waldo(n1, n2) returns True.

N1: your modulus
N2: your classmate's modulus
e: the encryption exponent

return d: the decryption exponent
"""
def get_private_key_from_n1_n2_e(n1, n2, e):
    # n1 = p * q_1, n2 = p * q_2; d := Decryption exponent
    d, p = e, gcd(n1, n2)
    q_1 = n1 // p
    euler_totient = (p - 1) * (q_1 - 1)
    # Calculate the decryption exponent (d = e^-1 mod (p-1)(q-1))
    return int(modinv(euler_totient, e))


def get_student_number():
    return "3035946760"
    
