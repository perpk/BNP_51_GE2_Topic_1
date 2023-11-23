# Load necessary libraries
library("igraph")
library("networkD3")
library("writexl")

# Load necessary functions from local sources
source("readToMatrix.R")
source("calculateDegree.R")
source("calculateBetweenness.R")

# Read file contents into a matrix.
A = readToMatrix('mouse.txt')

# Transform the data from the matrix to a non-directed network.
network <- graph.edgelist(A, directed = F)

# ---------------------------------------------------------------------

# Degree
## Calculate the degree and find the 2 proteins with the highest values. 
calculateDegree(network, N = 2, descending = TRUE)

## Represent all degrees of the network as a data frame with descending ordering
degreeData <- as.data.frame(calculateDegree(network, descending = TRUE))

# ---------------------------------------------------------------------

# Density
## Determine whether the network has loops
hasLoops <- any(which_multiple(network) == TRUE)

## Calculate the density
graph.density(network, loops = hasLoops)

# ---------------------------------------------------------------------

# Betweenness
## Calculate the betweenness and find the 2 proteins with the highest values
calculateBetweenness(network, descending = TRUE, N = 2)

## Represent all betweenness value of the network as a data frame with descending ordering
betweennessData <- as.data.frame(calculateBetweenness(network, descending = TRUE))

# ---------------------------------------------------------------------

# Clustering Coefficient
## Calculate the clustering coefficient via the transitivity function.
clusterCoefficient <- transitivity(network, "localundirected", isolates = c("NaN", "zero"))

## Find the top most proteins after applying descending ordering.
top <- order(clusterCoefficient, decreasing = TRUE)[1:2]

## Transform the clustering coefficient vector to a data frame.
clusterCoefficientData <- as.data.frame(clusterCoefficient)

# ---------------------------------------------------------------------

# Degree Distribution Histogram
## Calculate the degree for each node in the protein network.
allDegrees <- calculateDegree(network, descending = TRUE)

## Normalize the network degree values.
x <- rnorm(allDegrees)

## Plot the Histogram with the degree distribution.
hist(x, breaks = 30, main = 'Protein Network Degree Histogram', xlab = 'degrees')

# ---------------------------------------------------------------------

# Connected Components of the Network
cs <- components(network)
membershipData <- as.data.frame(cs$membership)
csizeData <- as.data.frame(cs$csize)
write_xlsx(csizeData, "csizeData.xlsx")

# top <- which.max(cs$csize)
topThree <- order(cs$csize, decreasing=TRUE)[1:3]

ranked1st <- topThree[1]
ranked2nd <- topThree[2]
ranked3rd <- topThree[3]

vertices1 <- V(network)[cs$membership == ranked1st]
vertices2 <- V(network)[cs$membership == ranked2nd]
vertices3 <- V(network)[cs$membership == ranked3rd]

subnet1 <- induced.subgraph(network, vertices1)
subnet2 <- induced.subgraph(network, vertices2)
subnet3 <- induced.subgraph(network, vertices3)

simpleNetwork(as_data_frame(subnet1), zoom = T)
simpleNetwork(as_data_frame(subnet2), zoom = T)
simpleNetwork(as_data_frame(subnet3), zoom = T, linkDistance = 120, opacity = 100)

# ---------------------------------------------------------------------

# Density for subnet #3
## Determine whether the network has loops
hasLoops <- any(which_multiple(subnet3) == TRUE)

## Calculate the density
graph.density(subnet3, loops = hasLoops)

# ---------------------------------------------------------------------

# Shortest Paths

sp_Q8CGI9_E9Q557 <- get.shortest.paths(network, 
                                       from = V(network)['Q8CGI9'], 
                                       to = V(network)['E9Q557'])

sp_P39428_Q3THG9 <- get.shortest.paths(network, 
                                       from = V(network)['P39428'], 
                                       to = V(network)['Q3THG9'])


# ---------------------------------------------------------------------



