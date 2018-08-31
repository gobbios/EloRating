#' matrix to sequence conversion
#' @aliases mat2seqint
#' @param mat square interaction matrix with winner in rows and losers in columns (can have column/row names or not)
#'
#' @return a data.frame with a winner and a loser column
#' @export
#'
#' @examples
#' mat <- matrix(c(0,1,1,0,0,1,0,0,0), ncol=3, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- LETTERS[1:3]
#' mat2seq(mat)
#'
#' mat <- matrix(c(0,1,1,0,0,1,3,0,0), ncol=3, byrow = TRUE)
#' rownames(mat) <- colnames(mat) <- LETTERS[1:3]
#' mat2seq(mat)
#'
#' # without column names
#' mat <- matrix(c(0,1,1,0,0,1,0,0,0), ncol=3, byrow = TRUE)
#' mat2seq(mat)
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib EloRating


mat2seq <- function(mat) {
  # create sequence
  res <- mat2seqint(mat)
  # return data frame either with ID names (if there are column names) or with indices
  if (is.null(colnames(mat))) {
    res <- data.frame(winner = res[[1]], loser = res[[2]])
  } else {
    res <- data.frame(winner = colnames(mat)[res[[1]]], loser = colnames(mat)[res[[2]]])
  }
  return(res)
}
