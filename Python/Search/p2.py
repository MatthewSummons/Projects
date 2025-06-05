import sys, grader, parse
from collections import deque

def bfs_search(problem):
    # Perform null checking to catch parsing errors
    if not(problem): return ""
    
    # Isolate the adjacent nodes and cost from the entry/end points and the heuristic
    startNode, endNodes, _ = problem.pop("Start"), problem.pop("End"), problem.pop("Heuristic")

    explorationOrder, pathOrder = "", ""
    # Perform DFS with specified startNode until one of the nodes in endNodes is reached
    frontier, visited = deque([startNode]), set()
    while frontier:
        path = frontier.popleft()
        node = path.split()[-1]
        if node in endNodes:
            return f"{explorationOrder[:-1]}\n{path}"
        if node not in visited:
            if node in problem:
                for _, adj in problem[node]:
                    frontier.append(path + ' ' + adj)
            explorationOrder += f"{node} "
            visited.add(node)
    return f"{explorationOrder}\nNo path from {startNode} to any of the following end nodes: {endNodes}"

if __name__ == "__main__":
    test_case_id = int(sys.argv[1])
    problem_id = 2
    grader.grade(problem_id, test_case_id, bfs_search, parse.read_graph_search_problem)