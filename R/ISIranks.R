#' ISI ranks
#'
#' @param x a list of matrices, with the same column names, typically the output of \code{\link{ISI}}
#' @param sortbyID logical, should the output be sorted by ID (default is \code{TRUE}). If \code{FALSE}, output is sorted by (average rank)
#'
#' @details if there is more than one solution resulting from \code{\link{ISI}}, average (mean) ranks will be calculated. If there is only one solution, the average rank will be the same as the rank from the (one) ISI ranking
#' @return a data.frame with at least three columns: IDs, their average rank and the rankings of all rankings that satisfy ISI's minimum criteria
#' @export
#'
#' @examples
#' # no unique solution
#' data(adv)
#' mat <- creatematrix(winners = adv$winner, losers = adv$loser)
#' set.seed(123)
#' res <- ISI(mat)
#' ISIranks(res)
#' ISIranks(res, sortbyID = FALSE)
#'
#' # only one (and unique) solution
#' data(bonobos)
#' set.seed(123)
#' res <- ISI(bonobos)
#' ISIranks(res)
#' ISIranks(res, sortbyID = FALSE)


ISIranks <- function(x, sortbyID = TRUE) {
  foo <- function(X) data.frame(id = colnames(X),
                                ISIrank = 1:length(colnames(X)))
  temp <- lapply(X = x, FUN = foo)
  temp <- lapply(temp, function(X) X[order(X[, 1]), ] )

  res <- data.frame(ID = temp[[1]][, 1], avg = NA)
  for (i in 1:length(temp)) {
    res <- cbind(res, temp[[i]][, 2])
    colnames(res)[ncol(res)] <- paste0("rnkg", i)
  }
  res$avg <- rowMeans(res[, 3:ncol(res), drop = FALSE])

  if (!sortbyID) res <- res[order(res$avg), ]

  rownames(res) <- NULL

  return(res)
}
