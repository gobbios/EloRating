# isi 15_07_05

#' difference matrix
#'
#' difference matrix
#'
#' @param mat square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#'
#' @return data frame with inconsistencies and their strength
#'
#' @author Christof Neumann
#'
#' @details helper function for \code{\link{ISI}}
#'
#' @examples
#' data(bonobos)
#' .diffmat(bonobos)
#'


.diffmat <- function(mat) {
  x<-ncol(mat)
  matrix(rep(0:x, x), ncol=x, byrow=T)[1:x, ]
}

