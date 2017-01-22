#' calculate David's scores
#'
#' calculate David's scores
#'
#' @param interactionmatrix square interaction matrix with winner in rows and losers in columns, for example the output from
#' @param prop the type of dyadic win proportion to be use. By default corrected for number of interactions in a dyad (\code{prop="Dij"}), otherwise the raw proportion (\code{prop="Pij"})
#'
#' @return a data.frame with columns ID, DS (David's scores) and normDS (normalized David's scores)
#'
#' @references David, H. A. 1987. Ranking from unbalanced paired-comparison data. Biometrika, 74, 432-436. (\href{https://dx.doi.org/10.1093/biomet/74.2.432}{DOI: 10.1093/biomet/74.2.432})
#'
#' de Vries, H., Stevens, J. M. G. & Vervaecke, H. 2006. Measuring and testing the steepness of dominance hierarchies. Animal Behaviour, 71, 585-592. (\href{https://dx.doi.org/10.1016/j.anbehav.2005.05.015}{DOI: 10.1016/j.anbehav.2005.05.015})
#'
#' Gammell, M. P., de Vries, H., Jennings, D. J., Carlin, C. M. and Hayden, T. J. 2003. David's score: a more appropriate dominance ranking method than Clutton-Brock et al.'s index. Animal Behaviour, 66, 601-605. (\href{https://dx.doi.org/10.1006/anbe.2003.2226}{DOI: 10.1006/anbe.2003.2226})
#'
#' @author Christof Neumann
#'
#' @examples
#' data(bonobos)
#' DS(bonobos)
#'
#' @export

# \code{\link{creatematrix}}

# library(EloRating)
# data(adv)
# el <- elo.seq(adv$winner, adv$loser, adv$Date)
# interactionmatrix <- m <- creatematrix(el)

DS <- function(interactionmatrix, prop=c("Dij", "Pij")){
  if(length(intersect(rownames(interactionmatrix), colnames(interactionmatrix))) < ncol(interactionmatrix)) stop("not a square matrix")
  if(length(intersect(rownames(interactionmatrix), colnames(interactionmatrix))) < nrow(interactionmatrix)) stop("not a square matrix")

  # create an index for the matrix cells with unobserved dyads and the matrix diagonal
  summatrix <- interactionmatrix + t(interactionmatrix); diag(summatrix) <- 0
  summatrix <- replace(summatrix, summatrix==0, NA); l1 <- which(is.na(summatrix), arr.ind=TRUE)

  # create matrix with Dij OR Pij Indices
  if(prop[1]=="Dij") propmatrix <- (interactionmatrix+0.5) / (t(interactionmatrix)+interactionmatrix+1)
  if(prop[1]=="Pij") propmatrix <- interactionmatrix / (t(interactionmatrix)+interactionmatrix)

  # replace Dij/Pij-values for the diagonal and unobserved dyads with zero (by definition)
  propmatrix <- replace(propmatrix, l1, 0)

  # calculate w, w2, l, l2, and then DS and normDS
  w  <- rowSums(propmatrix)
  w2 <- propmatrix %*% w
  l  <- rowSums(t(propmatrix))
  l2 <- t(propmatrix) %*% l
  DS <- w + w2 - l - l2
  normDS <- ((DS+((length(DS)) * (length(DS)-1))/2)) / length(DS)

  res <- data.frame(ID=rownames(interactionmatrix), DS=DS, normDS=normDS)
  res <- res[order(res$DS, decreasing=T), ]; rownames(res) <- NULL

  return(res)
}

#DS(m, "Pij")
