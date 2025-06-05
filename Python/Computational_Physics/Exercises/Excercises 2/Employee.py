# Employee.py
# Class to represent Employees in Generic Corpotate Company
# Created by Shaheer Ziya on UTC+08 23:22

class Employee:
    # Class Variable bruv
    numEmployees = 0
    
    def __init__(self, firstName: str, lastName: str, staffNum: int, salary: float) -> None:
        self.firstName = firstName
        self.lastName = lastName
        self.salary = salary
        self.staffNum = staffNum
        # Increment Employee Counter
        Employee.numEmployees += 1

    def display(self) -> None:
       print(f"{self.firstName} {self.lastName}; Staff Number: {self.staffNum} Salary is ${self.salary}")
    
    @staticmethod
    def getCount():
        return Employee.numEmployees

Joe = Employee("Joe", "Biden", 1, 12000)
Joe_Mama = Employee("Joe", "Biden-1", 1, 112000)

Joe.display()
Joe_Mama.display()
print(Employee.getCount())
