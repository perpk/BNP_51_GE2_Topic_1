#' Calculate the Betweenness for all nodes of a network.
#' 
#' @seealso [graph.edgelist()]
#' @seealso [order()]
#' @seealso [graph.edgelist()]
#' @param network A network
#' @param N A number representing amount of elements to return. All elements will be included in the return value if parameter is omitted.
#' @param descending A boolean determining the ordering direction. Default is a value of FALSE which yields ordering in ascending fashion.
#' @return A vector with the betweenness of nodes represented as double numeric values.
calculateBetweenness <- function(network, 
                                 N = length(network), 
                                 descending = FALSE) {
  between <- betweenness(network)
  maxBindices <- order(between, decreasing=descending)[1:N]
  return (between[maxBindices])
}