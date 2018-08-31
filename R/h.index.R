# h.index 15_05_20
# written by C. Neumann


#' linearity indices
#'
#' linearity indices
#'
#' @param interactionmatrix square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#' @param loops numeric, the number of randomizations to perform (by default: 1000)
#'
#' @return a data.frame with with values for the number of individuals in the matrix (N), linearity indices (h, h' and expected h), p-value, number of randomizations, and number of unknown and tied relationships.
#'
#' @references
#' \insertRef{appleby1983}{EloRating}
#'
#' \insertRef{devries1995}{EloRating}
#'
#' @details Note that the expected value of \emph{h} can also be calculated as 3/(N+1).
#' @author Christof Neumann
#'
#' @examples
#' data(bonobos)
#' h.index(bonobos)
#'
#' @export




h.index <- function(interactionmatrix, loops = 1000){
  # expected h can also be calculated as 3/(N+1), see MatMan helpfile...
  mat <- interactionmatrix
  # triangle indices
  mu <- upper.tri(mat)

  # transposed matrix
  tmat <- t(mat)

  # upper and lower values
  uv <- mat[mu]; lv <- tmat[mu]

  # position of unknowns and ties
  up <- which(uv == lv & uv == 0)
  tp <- which(uv == lv & uv != 0)

  # number of unknown and tied relationships
  unknown <- length(up)
  tied <- length(tp)

  # onezero matrix
  ozmat <- (mat - tmat > 0) + 1 - 1
  # replace tied with 0.5
  ozmat[mat - tmat == 0 & mat != 0] <- 0.5

  N <- ncol(mat)

  # and some generic matrices, and values
  zeromat <- mat - mat
  onemat <- zeromat + 1
  s <- c(0, 1); d <- sum(mu)
  h_0 <- h_r <- numeric(loops)


  # diverge depending on existence of unknown relationships
  if (unknown > 0) {
    for (i in 1:loops) {
      rmat <- ozmat #ozmat2
      rmat[mu][up] <- sample(s, unknown, replace = TRUE)
      rmat <- t(rmat)
      rmat[mu] <- (onemat - t(rmat))[mu]
      h_0[i] <- (12 / (N ^ 3 - N)) * sum( (rowSums(rmat) - (0.5 * (N - 1))) ^ 2 )

      rmat <- zeromat
      rmat[mu] <- sample(s, d, replace = TRUE)
      rmat <- t(rmat)
      rmat[mu] <- (onemat - t(rmat))[mu]
      h_r[i] <- (12 / (N ^ 3 - N)) * sum( (rowSums(rmat) - (0.5 * (N - 1))) ^ 2 )

    }

    pval <- sum(h_r >= h_0) / loops
  }

  if (unknown == 0) {
    h_0[] <- (12 / (N ^ 3 - N)) * sum( (rowSums(ozmat) - (0.5 * (N - 1))) ^ 2 )
    for (i in 1:loops) {
      rmat <- zeromat
      rmat[mu] <- sample(s, d, replace = TRUE)
      rmat <- t(rmat)
      rmat[mu] <- (onemat - t(rmat))[mu]
      h_r[i] <- (12 / (N ^ 3 - N)) * sum( (rowSums(rmat) - (0.5 * (N - 1))) ^ 2 )

    }
    pval <- sum(h_r >= h_0) / loops
  }

  ozmat[mat - tmat == 0] <- 0.5
  diag(ozmat) <- 0
  temp <- (rowSums(ozmat) - (0.5 * (N - 1))) ^ 2

  h_Index <- (12 / (N ^ 3 - N)) * sum(temp)

  ifelse(unknown < 1, h_index_devries <- h_Index, h_index_devries <- h_Index + ( (6 * unknown) / (N ^ 3 - N)) )

  expected_h <- mean(h_r)

  results <- data.frame(c("N", "h index", "h' index", "expected h", "p right", "randomizations", "tied", "unknown"),
                        c(N, round(h_Index, 4), round(h_index_devries, 4), round(expected_h, 4), round(pval, 4), loops, tied, unknown))
  colnames(results) <- c("variable", "value")


  if (results$value[5] == 0) results$value[5] <- 1 / loops

  return(results)
}
