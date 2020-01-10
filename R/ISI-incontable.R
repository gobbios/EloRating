# isi 15_07_05

#' number and strength of inconsistencies
#'
#' calculate number and strength of inconsistencies
#'
#' @param mat square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#'
#' @return data frame with inconsistencies and their strength
#'
#' @author Christof Neumann
#'
#' @references de Vries, H. 1998. Finding a dominance order most consistent with a linear hierarchy: a new procedure and review. Animal Behaviour, 55, 827-843. (\href{https://dx.doi.org/10.1006/anbe.1997.0708}{DOI: 10.1006/anbe.1997.0708})
#'
#' @export
#'
#' @examples
#' data(bonobos)
#' incontable(bonobos)
#'

incontable <- function(mat) {
  # gives a table with all inconsistent dyads and the respective SI
  # - depends on ".incon"
  if (.incon(mat) == 0) {
    res <- as.data.frame(matrix(NA, nrow = 0, ncol = 3))
    colnames(res) <- c("too.low", "too.high", "SIs")
    message("no inconsistencies found")
  }

  if (.incon(mat) > 0) {
    N <- nrow(mat)
    res <- matrix(0, nrow = 2, ncol = 0)

    for (i in 1:(N - 1)) {
      ROW <- mat[i, ][upper.tri(mat)[i, ]]
      COL <- mat[, i][lower.tri(mat)[, i]]
      res <- cbind(res, rbind(names(which(ROW - COL < 0)),
                              rep(colnames(mat)[i],
                                  length(names(which(ROW - COL < 0))))))
    }

    SIs <- vector()
    for (i in 1:length(res[1, ])) {
      SIs <- c(SIs, which(rownames(mat) == res[1, i]) - which(rownames(mat) == res[2, i]))
    }

    res <- as.data.frame(t(rbind(res, SIs)))
    colnames(res)[1:2] <- c("too low", "too high")
    res[, 3] <- as.numeric(as.character(res[, 3]))
  }

  return(res)
}
