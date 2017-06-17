#############################################################################################################
#     Calculate the edit distance between two graphs
#
#     We calculate d <- min #(flips, insertions, deletions to transform 1 graph to the other) / # possible edges
#
#
# graph1:     Matrix where the i,j =1, implies that j is a parent of i
# graph2:     Matrix where the i,j =1, implies that j is a parent of i
#
#############################################################################################################

getDist <- function(graph1, graph2) {
  allEdgeMatch <- graph2 != graph2
  hammingDist <- sum(pmax(allEdgeMatch, t(allEdgeMatch))[lower.tri(allEdgeMatch)]) / (dim(graph1)[1] * (dim(graph1)[1] - 1) / 2)
}