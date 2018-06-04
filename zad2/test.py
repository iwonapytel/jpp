import random

def dfs(graph, start, visited=[], is_choice=True):
    if (start in visited):
        return visited
    visited.append(start)
    nbrs = graph[start][1:]

    if (graph[start][0] == 'e' and len(nbrs) > 1 and is_choice):
        nbrs = random.sample(nbrs, 1)
    
    for i in set(nbrs) - set(visited):
        dfs(graph, i, visited)

    return visited

def is_dfs(graph, dfs_list):
    visited = set(dfs_list[0])
    stack = [dfs_list[0]]
    for i in dfs_list[1:]:
        while(len(stack) > 0 and (not i in graph[stack[-1]][1:])):
            stack.pop()
        if (len(stack) == 0 or i in visited):
            return False
        if (graph[stack[-1]][0] == 'e'):
            graph[stack[-1]] == ['e']
        visited.add(i)
        stack.append(i)
    return True

def random_graph(length):
    types = ['a', 'e']
    vertices = ['v{}'.format(i) for i in range(length)]
    neighbours = [random.sample(types, 1) + random.sample(vertices, random.randint(0, length)) for i in range(length)]
    return dict(zip(vertices, neighbours))

def dump_graph(graph):
    for i in graph:
        graph[i].insert(0, i)
    return str(graph.values())

graph = random_graph(5)
print(graph)
dfs_list = list(dfs(graph, 'v0'))
print(dfs_list)
print(is_dfs(graph, dfs_list))
print(dump_graph(graph))

