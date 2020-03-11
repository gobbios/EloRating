#' changes in dyadic relationships
#'
#' compare dyadic relationships before and after a certain date
#'
#' @param eloobject result from \code{\link{elo.seq}}
#' @param cutpoint character or Date, the date at which to split into pre and
#'                 post (default is \code{NULL}, where the data is split in
#'                 halves). The actual date here will be included in the 'pre'
#'                 period.
#' @param daterange character or Date of length 2, the date range to be
#'                  considered (default is \code{NULL} where the entire date
#'                  range in the data is used)
#'
#' @return a data.frame with one line per dyad:
#' \describe{
#' \item{id1,id2}{the dyad}
#' \item{pre_n,post_n}{the number of interactions for that dyad pre and
#'                     post cutpoint date}
#' \item{pre,post}{which of the two was dominant (\code{1} = id1, \code{2} =
#'                 id2, \code{0} = tied relationship, \code{NA} = unknown
#'                 relationship, i.e. 0 interactions)}
#' }
#' @export
#'
#' @examples
#' data(adv)
#' eloobject <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' # split at halfway point ("2010-01-17")
#' # one reversal: a-f
#' dyadic_reversals(eloobject)
#' # shift split date so that both interactions for a/f occur in the post period,
#' # which makes it a tie in post and unknown in pre
#' dyadic_reversals(eloobject, cutpoint = "2010-01-10")
dyadic_reversals <- function(eloobject, cutpoint = NULL, daterange = NULL) {
  if (is.null(daterange)) {
    daterange <- range(eloobject$truedates)
  } else {
    daterange <- as.Date(as.character(daterange))
  }
  if (is.null(cutpoint)) {
    cutpoint <- mean(daterange)
  } else {
    cutpoint <- as.Date(as.character(cutpoint))
  }

  ltab <- eloobject$logtable
  ltab$D <- eloobject$truedates[ltab$Date]
  ltab <- ltab[ltab$D >= daterange[1] & ltab$D <= daterange[2], ]
  # remove draws
  ltab <- ltab[!ltab$draw, ]
  # refactor (so winners and losers have same levels)
  allids <- eloobject$allids
  ltab$winner <- factor(as.character(ltab$winner), levels = allids)
  ltab$loser <- factor(as.character(ltab$loser), levels = allids)

  winner_pre <- ltab$winner[ltab$D <= cutpoint]
  loser_pre <- ltab$loser[ltab$D <= cutpoint]
  winner_post <- ltab$winner[ltab$D > cutpoint]
  loser_post <- ltab$loser[ltab$D > cutpoint]

  dyads <- t(combn(allids, 2))
  # process dyadic relationships
  pretab <- table(winner_pre, loser_pre)
  posttab <- table(winner_post, loser_post)

  rel <- function(dyad, xtab) {
    dyadic <- c(xtab[dyad[1], dyad[2]], xtab[dyad[2], dyad[1]])
    if (dyadic[1] == dyadic[2]) {
      if (sum(dyadic) == 0) {
        return(NA)
      } else {
        return(0)
      }
    } else {
      if (dyadic[1] > dyadic[2]) {
        return(1)
      } else {
        return(2)
      }
    }
  }
  pre <- apply(dyads, 1, rel, xtab = pretab)
  post <- apply(dyads, 1, rel, xtab = posttab)
  pre_n <- apply(dyads, 1, function(y) sum(pretab[y, rev(y)]))
  post_n <- apply(dyads, 1, function(y) sum(posttab[y, rev(y)]))

  res <- data.frame(id1 = dyads[, 1],
                    id2 = dyads[, 2],
                    pre_n = pre_n,
                    pre = pre,
                    post_n = post_n,
                    post = post,
                    stringsAsFactors = FALSE)
  # classify transitions
  res$changed <- res$pre != res$post
  # when one period is 0 (a non-zero tie) for a relationship, set change to NA
  res$changed[res$post == 0 | res$pre == 0] <- NA

  res
}
