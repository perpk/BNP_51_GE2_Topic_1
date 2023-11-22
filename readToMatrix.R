#' Read file contents into a NxM Matrix.
#' @seealso [read.table()] for reading file data in tabular shape.
#' @seealso [as.matrix()] for transforming tabular data into a NxM Matrix.
#' 
#' @param filename string
#' @returns A Matrix
#' @examples
#' readToMatrix('filename.txt')

readToMatrix <- function(filename) {
  rawData <- read.table(filename, header = FALSE)
  return (as.matrix(rawData))
}