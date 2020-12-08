from collections import deque
import networkx as nx
import itertools
#import matplotlib.pyplot as plt
'''
This program contains data for several actors and movies. The user types in an actor name
and the program then calculates how many degrees the actor is from Kevin Bacon. 
It utilizes a graph and priority queue in order to calculate this. 
The user can continue to enter actor names unless they press enter to end the program. 
If the actor entered is not found, the program returns an error message.
'''


def getGraph():
    '''
    getGraph() is a method that creates a Graph, and then reads in lines from "actors", "movies", and "movie-actors" in order to populate and build the Graph. The edges are a pair of actors in the movie and the movie title. It closes each file after the contents are read in.
    '''
    G = nx.Graph()

    AIDtoName = {}
    file = open(r"actors.txt", "r", encoding="latin-1")
    for line in file.readlines():
        entry = line.strip().split("|")
        actorID = entry[0]
        actorName = entry[1]
        AIDtoName[actorID] = actorName
    file.close()

    MIDtoName = {}
    file = open(r"movies.txt", "r", encoding="latin-1")
    for line in file.readlines():
        entry = line.strip().split("|")
        movieID = entry[0]
        movieName = entry[1]
        MIDtoName[movieID] = movieName
    file.close()

    file = open(r"movie-actors.txt", "r", encoding="latin-1")
    myDict = {}
    for line in file.readlines():
        # delim pipe |
        entry = line.strip().split("|")
        movieID = entry[0]
        actorID = entry[1]
        actorName = AIDtoName.get(actorID)
        movieName = MIDtoName.get(movieID)
        # does it already exist in the dictionary?
        if myDict.get(movieName):
            # myDict.get(movieID) is the LIST of things for that movie
            myDict.get(movieName).append(actorName)
        else:
            # add to dict
            myDict[movieName] = [actorName]
    file.close()

    for movie in list(myDict):
        actorList = myDict[movie]
        # gives us all movies & actor combinations
        for combination in (list(itertools.combinations(actorList, 2))):
            G.add_edge(combination[0], combination[1], movie=movie)
    return G


def bfs(g, start):
    '''
    bfs() is a method that does a breadth-first search through the graph until the queue is empty. It pops() each vertex V (currV) and finds all adjacent neighbors, then adds the edge to the discovered_set.
    parameters: graph, start point (in this case, Kevin Bacon)
    returns: discovered_set
    '''
    # insert root into an empty queue Q and into a new directed graph T
    # until Q is empty
    #   dequeue Q to get next vertex v to process
    #     for each edge e that is incident to v in G
    #       let v' be the other end of the edge
    #       if v' is not in T
    #         add v' to T and add an edge with the same label as e from v' to v in T
    #         enqueue v' in Q
    # return T
    frontier_queue = deque()
    frontier_queue.appendleft(start)
    discovered_set = nx.DiGraph()

    while len(frontier_queue) > 0:
        currV = frontier_queue.pop()
        for adjV in list(g.neighbors(currV)):
            if adjV not in discovered_set:
                frontier_queue.appendleft(adjV)
                discovered_set.add_edge(
                    currV, adjV, movie=g[currV][adjV]["movie"])
    return discovered_set


def __main__():
    Graph = getGraph()
    bfsGraph = bfs(Graph, 'Kevin Bacon')

    while True:
        # TODO: Handle for when there is no path between 2 actors
        # try block goes here
        try:
            print("To quit the program, type return in answer to a question.\n")
            actorName = input("Enter the name of an actor: ")
            shortest_path = nx.shortest_path(
                bfsGraph, 'Kevin Bacon', actorName)
            # we only care for shortest path
            shortestPathList = list(shortest_path)
            shortestPathList.reverse()
            # num is length of shortestPath - 1
            print("{0}'s number is {1}".format(
                actorName, len(shortest_path)-1))
            for x in range(len(shortestPathList)-1):
                print("{0} appeared in {1} with {2}".format(
                    shortestPathList[x], bfsGraph[shortestPathList[x+1]][shortestPathList[x]]["movie"], shortestPathList[x+1]))
        except:
            print("This actor has no connection to Kevin Bacon.")
        if not actorName:
            print("Exiting program...")
            break

# TODO: Add 2+ interesting statistics about this

# TODO: Graph
# nxg = nx.Graph()
# nxg.add_nodes_from(["A", "B", "C", "D"])
# # and so on...
# # nxg.add_edges_from([("A", "B", {weight: 3}]), [("A", "C", {'weight': 7})])
# pos = nx.spring_layout(nxg)
# nx.draw(nxg, pos, with_labels="True")
# nx.draw_networkx_edge_labels(nxg, pos, edge_labels={"Here are labels"})
# plt.show
__main__()
