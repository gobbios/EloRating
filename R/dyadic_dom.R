#' dyadic dominance relations
#'
#' @param winner character or factor with winner
#' @param loser character or factor with winner
#' @param Date not yet implemented
#' @param daterange not yet implemented
#'
#' @return a data.frame with one row per dyad
#' @export
#'
#' @examples
#' xdata <- randomsequence(nID = 5, avgIA = 10, reversals = 0.1)$seqdat
#' dyadic_dom(xdata$winner, xdata$loser)

dyadic_dom <- function(winner, loser, Date = NULL, daterange = NULL) {
  # data prep
  winner <- as.character(winner)
  loser <- as.character(loser)
  allids <- sort(unique(c(winner, loser)))

  xtab <- table(factor(winner, levels = allids),
                factor(loser, levels = allids))

  dyads <- t(combn(allids, 2))
  whosdom <- character(nrow(dyads))
  firstwins <- numeric(nrow(dyads))
  secondwins <- numeric(nrow(dyads))

  for (i in seq_along(whosdom)) {
    # first wins
    x1 <- xtab[dyads[i, 1], dyads[i, 2]]
    # second wins
    x2 <- xtab[dyads[i, 2], dyads[i, 1]]
    if (x1 > x2) whosdom[i] <- dyads[i, 1]
    if (x2 > x1) whosdom[i] <- dyads[i, 2]
    if (x1 == x2) whosdom[i] <- NA
    firstwins[i] <- x1
    secondwins[i] <- x2
    rm(x1, x2)
  }

  data.frame(id1 = dyads[, 1],
             id2 = dyads[, 2],
             id1_wins = firstwins,
             id2_wins = secondwins,
             dom = whosdom,
             stringsAsFactors = FALSE)
}
