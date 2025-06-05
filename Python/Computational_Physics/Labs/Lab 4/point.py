# point.py
# Written by F K Chow, HKU
# Modified by Shaheer Ziya, HKU
# Last update: 2022/4/6

class Point:
    """ A class representing geometric points """
    
    def __init__(self, x: float, y: float) -> None:
        """ Initialize the point with its x, y coordinates """
        self.x = x
        self.y = y

    def getx(self) -> float:
        """ Return the x coordinate of the point """
        return self.x

    def gety(self) -> float:
        """ Return the y coordinate of the point """
        return self.y

    def __str__(self) -> str:
        """ Return a string representation of the point """
        return '({:.1f}, {:.1f})'.format(self.x, self.y)
