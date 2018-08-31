#' David's score
#'
#' calculate David's scores from an interaction matrix
#'
#' @param interactionmatrix square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#' @param prop the type of dyadic win proportion to be use. By default corrected for number of interactions in a dyad (\code{prop="Dij"}), otherwise the raw proportion (\code{prop="Pij"})
#'
#' @return a data.frame with columns ID, DS (David's scores) and normDS (normalized David's scores)
#'
#' @references
#' \insertRef{david1987}{EloRating}
#'
#' \insertRef{gammell2003}{EloRating}
#'
#' \insertRef{devries2006}{EloRating}
#'
#' @author Christof Neumann
#'
#' @examples
#' data(bonobos)
#' DS(bonobos)
#' DS(bonobos, prop = "Pij")
#'
#' @export

DS <- function(interactionmatrix, prop = c("Dij", "Pij")) {
  rn <- rownames(interactionmatrix)
  cn <- colnames(interactionmatrix)
  merged <- length(intersect(rn, cn))
  if (merged < ncol(interactionmatrix)) stop("not a square matrix")
  if (merged < nrow(interactionmatrix)) stop("not a square matrix")

  # create an index for the matrix cells with unobserved dyads and the matrix diagonal
  tmat <- t(interactionmatrix)
  summatrix <- interactionmatrix + tmat
  diag(summatrix) <- 0
  summatrix <- replace(summatrix, summatrix == 0, NA)
  l1 <- which(is.na(summatrix), arr.ind = TRUE)

  # create matrix with Dij OR Pij dyadic indices
  if (prop[1] == "Dij") {
    propmatrix <- (interactionmatrix + 0.5) / (tmat + interactionmatrix + 1)
  }
  if (prop[1] == "Pij") {
    propmatrix <- interactionmatrix / (tmat + interactionmatrix)
  }

  # replace Dij/Pij-values for the diagonal and unobserved dyads with zero (by definition)
  propmatrix <- replace(propmatrix, l1, 0)

  # calculate w, w2, l, l2, and then DS and normDS
  w  <- rowSums(propmatrix)
  w2 <- propmatrix %*% w
  l  <- rowSums(t(propmatrix))
  l2 <- t(propmatrix) %*% l
  DS <- w + w2 - l - l2
  normDS <- ( (DS + ( (length(DS)) * ( length(DS) - 1) ) / 2) ) / length(DS)

  res <- data.frame(ID = rn, DS = DS, normDS = normDS)
  res <- res[order(res$DS, decreasing = TRUE), ]
  rownames(res) <- NULL

  return(res)
}
