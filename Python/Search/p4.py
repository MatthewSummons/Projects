import sys, parse, grader
from heapq import heappop, heappush

def greedy_search(problem):
    # Perform null checking to catch parsing errors
    if not(problem): return ""
    
    # Isolate the adjacent nodes and cost from the entry/end points and the heuristic
    startNode, endNodes = problem.pop("Start"), problem.pop("End")
    heuristic = problem.pop("Heuristic")

    explorationOrder, pathOrder = "", ""
    # Perform DFS with specified startNode until one of the nodes in endNodes is reached
    frontier, visited = [], set()
    heappush(frontier, (heuristic[startNode], startNode))
    
    while frontier:
        path:list[tuple[float, str]] = heappop(frontier)        # A tuple of the form (cost, path of nodes)
        node = path[1].split()[-1] 
        if node in endNodes:
            return f"{explorationOrder[:-1]}\n{path[1]}"
        if node not in visited:
            if node in problem:
                for _, adj in problem[node]:
                    heappush(frontier, (heuristic[adj], path[1] + ' ' + adj))
            explorationOrder += f"{node} "
            visited.add(node)
    return f"{explorationOrder}\nNo path from {startNode} to any of the following end nodes: {endNodes}"

if __name__ == "__main__":
    test_case_id = int(sys.argv[1])
    problem_id = 4
    grader.grade(problem_id, test_case_id, greedy_search, parse.read_graph_search_problem)