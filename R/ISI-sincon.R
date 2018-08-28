# isi 15_07_05

#' strength of inconsistencies
#'
#' calculate strength of inconsistencies
#'
#' @param mat square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#'
#' @return integer, the summed strength of inconsistencies in the matrix
#'
#' @author Christof Neumann
#'
#' @references de Vries, H. 1998. Finding a dominance order most consistent with a linear hierarchy: a new procedure and review. Animal Behaviour, 55, 827-843. (\href{https://dx.doi.org/10.1006/anbe.1997.0708}{DOI: 10.1006/anbe.1997.0708})
#'
#' @details helper function for \code{\link{ISI}}
#'
#' @examples
#' data(bonobos)
#' EloRating:::.sincon(bonobos)
#'


.sincon <- function(mat) {
  dmat <- .diffmat(mat)
  sum(dmat[upper.tri(mat)][mat[upper.tri(mat)] < t(mat)[upper.tri(mat)]],
      na.rm = TRUE)
}
