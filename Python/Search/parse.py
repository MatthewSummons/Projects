import os, sys


'''
Reads a problem file and returns a dictionary with a fixed format represnting the problem statement laid
out in the assignment handout.

The dictionary contains three special keys, namely Start, End and Heuristic. Start is the entry point 
of the graph, End contains a list of all nodes that can be considered a goal, Heuristic holds a dictionary
that maps each node to its heuristic value. The remaining entries are the edges and the cost of the edges in
the graph.

We assume that no node is called Start, End or Heuristic, among other trivial assumptions about the content of
the files that are read. If the file provided in file_path is not formatted correctly, the behaviour of this
function is undefined.
'''
def read_graph_search_problem(file_path:str) -> dict | None:
    try: file = open(file_path, "r")
    except OSError as e: return print(e)
    
    problem = {}
    
    # First line denotes a single starting node
    problem["Start"] = file.readline().split()[1]
    # Second line consists of a list of goal node separated by a space
    problem["End"] = file.readline().split()[1:]

    problem["Heuristic"] = {} 
    # Lines with only two words are ( <state>, <heuristic> ) pairs
    line = file.readline().split()
    while len(line) == 2:
        if line[0] not in problem:
            problem["Heuristic"][line[0]] = float(line[1])
        else:
            return print("A node must have a unique heuristic value")
        line = file.readline().split()
    
    # The remaining lines indicate transition costs, i.e. ( <start state>, <end state>, <cost> ) tuples
    while line:
        # This part builds the nighbourhoods of nodes in (cost, node) pairs
        if line[0] in problem:
            problem[line[0]].append( (float(line[2]), line[1]) )
        else:
            problem[line[0]] = [ (float(line[2]), line[1]) ]
        line = file.readline().split()
    file.close()
    return problem

def read_8queens_search_problem(file_path:str) -> list[tuple[int,int]]:
    with open(file_path, 'r') as file:
        # Read the board, eliminating newlines and spaces
        data = file.read().replace('\n', '').replace(' ', '')
    # Identify the (col, row) of queens
    queenPositions = [(i % 8, i // 8) for i, char in enumerate(data) if char == 'q']
    # Sort the queens by column
    queenPositions.sort()
    return queenPositions
    

if __name__ == "__main__":
    if len(sys.argv) == 3:
        problem_id, test_case_id = sys.argv[1], sys.argv[2]
        if int(problem_id) <= 5:
            problem = read_graph_search_problem(os.path.join('test_cases','p'+problem_id, test_case_id+'.prob'))
        else:
            problem = read_8queens_search_problem(os.path.join('test_cases','p'+problem_id, test_case_id+'.prob'))
        print(problem)
    else:
        print('Error: I need exactly 2 arguments!')