# Quadratic Class

class Quadratic:
    def __init__(self, a, b, c):
        self.a = a
        self.b = b
        self.c = c
    
    def value(self, x):
        return self.a * (x*x) + self.b * x  + self.c
    
    def table(self, n, L, R):
        # List to store the table
        table = []
        # Assign x to be the value of point in table, starting with L
        x = L
        while x < R:
            table.append((x, self.value(x)))
            x += (R - L) / n
        
        print(f"{'x':^17} | {'f(x)':^17}")
        for coord in table:
            print(f"{coord[0]:^17.3f} | {coord[1]:^17.3f}")


# Q1 = Quadratic(1,0,0) 
# Q1.table(100,0,100)       