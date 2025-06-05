import sys, parse, grader

def number_of_attacks(queens:list[tuple[int, int]], returnMatrix:bool = False) -> str | tuple[list[int]]:
    '''
    We wish to calculate the number of attacks between queens for each possible
    neighbouring state of the current state. A neighbour state is acheived by moving a single
    queen to a different square along the same column. The number of attacks between queens is
    any direct/indirect pairs of queens that lie in the same row, column or diagonal. The number
    of attacks between queens is calculated for each possible neighbouring state.
    
    Break this problem into two parts:
        1. Count the number of attacks between queens in "other" columns
        2. Count the number of attacks between queens in the same column as the queen being moved

    Observe that the number of attacks between queens in "other" columns is constant for all
    neighbouring states in the same column.
    '''
    numRows, numCols, numQueens = 8, 8, len(queens)
    neighbours = tuple([0] * numCols for _ in range(numRows))
    for col in range(numCols):
        # Count attacks between queens in "other" columns
        colInv = 0
        for Q1 in range(numQueens):
            if Q1 == col: continue      # Skip the queen being moved
            for Q2 in range(Q1 + 1, numQueens):
                if Q2 != col and attack(queens[Q1], queens[Q2]):
                    colInv += 1
        for row in range(numRows):
            neighbours[row][col] = colInv
            # Count attacks between moved queen and the rest
            for Q in range(numQueens):
                if Q != col and attack(queens[Q], (col, row)):
                    neighbours[row][col] += 1
    
    if returnMatrix:
        return neighbours
    
    # Convert the neighbour state matrix into the desired string if function is not asked to return matrix
    result = ""
    for row in range(numRows):
        for col in range(numCols):
            result += f"{neighbours[row][col]: ^3}"
        result = result[:-1] + "\n"
    return result[:-1]

def attack(queen1, queen2) -> bool:
    # Check if two queen attack each other by checking rows, columns and diagonals
    return queen1[0] == queen2[0] or queen1[1] == queen2[1] or \
    abs(queen1[0] - queen2[0]) == abs(queen1[1] - queen2[1])

if __name__ == "__main__":
    test_case_id = int(sys.argv[1])
    problem_id = 6
    grader.grade(problem_id, test_case_id, number_of_attacks, parse.read_8queens_search_problem)