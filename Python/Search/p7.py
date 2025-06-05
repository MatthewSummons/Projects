import sys, parse, grader
from p6 import number_of_attacks

def better_board(problem):
    neighbourStates = number_of_attacks(problem, returnMatrix=True)
    numRows, numCols = len(neighbourStates), len(neighbourStates[0])
    
    # Locate the first minimum value in the neighbour state matrix (iterating over rows from left to right)
    # An ideal place to use argmin from numpy but alas it's not permitted for this assignment
    minVal, minRow, minCol = neighbourStates[0][0], 0, 0
    for r, _ in enumerate(neighbourStates):
        for c, _ in enumerate(neighbourStates[r]):
            if neighbourStates[r][c] < minVal:
                minVal, minRow, minCol = neighbourStates[r][c], r, c
    
    # Note that problem holds the queen in column sorted order, so index is the same as column
    # Move the queen in the selected column to the position which minimizes the number of attacks
    problem[minCol] = (minCol, minRow)

    # Convert the queen positions to a string and return it
    result = ""
    for i in range(numRows):
        for j in range(numCols):
            if (j, i) == problem[j]: result += "q "
            else: result += ". "
        result = result[:-1] + "\n" # Remove the trailing space and add a newline
    return result[:-1] # Remove the trailing newline


if __name__ == "__main__":
    test_case_id = int(sys.argv[1])
    problem_id = 7
    grader.grade(problem_id, test_case_id, better_board, parse.read_8queens_search_problem)