#' Calculate the Degree for all nodes of a network.
#' 
#' @seealso [graph.edgelist()]
#' @seealso [order()]
#' @seealso [graph.edgelist()]
#' @param network A network
#' @param N A number representing amount of elements to return. All elements will be included in the return value if parameter is omitted.
#' @param descending A boolean determining the ordering direction. Default is a value of FALSE which yields ordering in ascending fashion.
#' @return A vector with the degrees of nodes represented as double numeric values.
calculateDegree <- function (network, N = length(network), descending = FALSE) {
  netDegree <- degree(network)
  maxIndices <- order(netDegree, decreasing = descending)[1:N]
  return (netDegree[maxIndices])
}