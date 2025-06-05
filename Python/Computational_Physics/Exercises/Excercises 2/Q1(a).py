# Spehere Class
import math

class Sphere:
    def __init__(self, radius):
        self.radius = radius
    
    def getRadius (self):
        return self.radius
    
    def SurfaceArea (self):
        # SA = 4πr^2
        return 4 * math.pi * (self.radius * self.radius)
    
    def Volume (self):
        # Vol = 4/3 π ^3
        return (4 / 3) * math.pi * (self.radius * self.radius * self.radius)

s1 = Sphere(1)
print(s1.getRadius())


