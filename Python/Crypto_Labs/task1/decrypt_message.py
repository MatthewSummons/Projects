
"""
N: the modulus
e: the encryption exponent
d: the decryption exponent
c: the encrypted message

return m: the plain text
"""
def decrypt_message(N, e, d, c):
    # Message = c^d mod N
    m = pow(c, d, N)
    return hex(m).rstrip('L')

def get_student_number():
    return "3035946760"
    
