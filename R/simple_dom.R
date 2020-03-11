#' simple dominance indices
#'
#' @param winner character or factor with winner
#' @param loser character or factor with winner
#' @param Date not yet implemented
#' @param daterange not yet implemented
#' @details The indices that are calculated are the following
#' \describe{
#'   \item{\code{winprop}}{the proportion of all interactions won}
#'   \item{\code{domover}}{the proportion of individuals dominated (regardless of whether any interactions may have occured, i.e. the number of individuals dominated is divided by N - 1 for all individuals)}
#'   \item{\code{domover_rel}}{the proportion of individuals dominated with which the focal interacted}
#' }
#' @return a data.frame with one row per individual and several 'simple' dominance indices
#' @export
#'
#' @examples
#' xdata <- randomsequence(nID = 10, avgIA = 20, reversals = 0.2)$seqdat
#' simple_dom(xdata$winner, xdata$loser)

simple_dom <- function(winner,
                       loser,
                       Date = NULL,
                       daterange = NULL) {
  # data prep
  winner <- as.character(winner)
  loser <- as.character(loser)
  allids <- sort(unique(c(winner, loser)))

  xtab <- table(factor(winner, levels = allids),
                factor(loser, levels = allids))
  mat <- matrix(xtab, ncol = ncol(xtab))
  # simple win proportion
  winprops <- rowSums(mat) / (rowSums(mat) + colSums(mat))
  winprops_rank <- rank(winprops * (-1), ties.method = "average")

  # dominated ids considering all potential opponents
  domover <- rowSums((mat - t(mat)) > 0) / (ncol(mat) - 1)
  domover_rank <- rank(domover * (-1), ties.method = "average")

  # dominated ids considering only realized opponents
  namat <- mat + t(mat)
  mat[namat == 0] <- NA
  domover_rel <- rowSums((mat - t(mat)) > 0, na.rm = TRUE) /
    rowSums(!is.na(mat))
  # alternative: apply(mat, 1, function(y) sum(!is.na(y)))
  rowSums(!is.na(namat))
  domover_rel_rank <- rank(domover_rel * (-1), ties.method = "average")

  res <- data.frame(ID = allids,
                    winprop = winprops,
                    winprop_rank = winprops_rank,
                    domover = domover,
                    domover_rank = domover_rank,
                    domover_rel = domover_rel,
                    domover_rel_rank = domover_rel_rank,
                    stringsAsFactors = FALSE)
  rownames(res) <- NULL
  res
}
