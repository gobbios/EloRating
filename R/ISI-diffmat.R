# isi 15_07_05

#' difference matrix
#'
#' difference matrix
#'
#' @param mat square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#'
#' @return a matrix with ranking differences assuming that the matrix reflects the order. This information is contained in the upper triangle of the returned matrix.
#'
#' @author Christof Neumann
#'
#' @details helper function for \code{\link{ISI}}
#'
#' @examples
#' data(bonobos)
#' EloRating:::.diffmat(bonobos)
#'

.diffmat <- function(mat) {
  x <- ncol(mat)
  matrix(rep(0:x, x), ncol = x, byrow = TRUE)[1:x, ]
}
