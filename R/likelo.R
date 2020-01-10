#' (log) likelihood of Elo-rating model
#'
#' @param eloobject output from \code{\link{elo.seq}} (or from \code{\link{fastelo}})
#' @param daterange character or Date of length 2, gives the date range for which likelihood should be calculated. By default, the entire date range of all interactions is considered.
#' @param ll logical, should the log likelihood be returned rather than the likelihood, by default \code{TRUE}
#' @param burnin numeric, the number of interactions to be excluded from the calculation of the (log) likelihood. This parameter is ignored if a date range is supplied. By default \code{burnin = 0}, i.e. all interactions are considered.
#' @details This function returns the (log) likelihood of a dominance interaction sequence. The likelihood is the product of all winning probabilities (for each interaction).
#' @return numeric of length 1, the (log) likelihood
#' @export
#'
#' @references
#' \insertRef{franz2015a}{EloRating}
#'
#' \insertRef{mcmahan1984}{EloRating}
#' @examples
#' data(adv)
#' res <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date, k = 200)
#' likelo(res)
#' res <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date, k = 100)
#' likelo(res)
#' ks <- seq(100, 400, by = 20)
#' liks <- numeric(length(ks))
#' for(i in 1:length(liks)) {
#'   liks[i] <- likelo(elo.seq(winner = adv$winner, loser = adv$loser,
#'                     Date = adv$Date, k = ks[i]))
#' }
#' plot(ks, liks, type = "l")
#'
#' # discard early interactions via 'burnin'
#' likelo(res)
#' # the same as above:
#' likelo(res, burnin = 0)
#' # discard the first 10 interactions:
#' likelo(res, burnin = 10)
#' # discard all but the last interaction:
#' likelo(res, burnin = 32)
#' # which is the same as the log of the last winning probability:
#' log(winprob(res$logtable$Apre[33], res$logtable$Bpre[33]))

likelo <- function(eloobject,
                   burnin = 0,
                   ll = TRUE,
                   daterange = NULL) {
  res <- NA

  # two 'methods': one for classic 'elo' and for 'fastelo'
  if (inherits(x = eloobject, what = "elo")) {
    temp <- eloobject$logtable
    normprob <- eloobject$misc["normprob"] == "1"
    if (!is.null(daterange)) {
      d1 <- which(eloobject$truedates == as.Date(daterange[1]))
      d2 <- which(eloobject$truedates == as.Date(daterange[2]))
      temp <- temp[temp$Date >= d1 & temp$Date <= d2, ]
      if (!is.null(burnin)) {
        burnin <- NULL
        message("burnin ignored because date range supplied")
      }
    }
    foo <- function(X) winprob(elo1 = X[1], elo2 = X[2], normprob = normprob)
    winprobs <- apply(temp[, c("Apre", "Bpre")], MARGIN = 1, FUN = foo)
    nint <- length(winprobs)
    if (burnin >= nint) {
      stop (paste("not enough interactions (",
                  nint,
                  ") for the desired burnin (",
                  burnin,
                  ")",
                  sep = ""),
            call. = FALSE)
    }
    startloc <- burnin + 1

    if (ll) {
      res <- sum(log(winprobs[startloc:nint]))
    } else {
      res <- prod(winprobs[startloc:nint])
    }
  }

  if (inherits(x = eloobject, what = "fastelo")) {
    if (!is.null(daterange)) {
      message ("date range ignored for results from fastelo")
    }
    nint <- length(eloobject$winprobs)
    if (burnin >= nint) {
      stop (paste("not enough interactions (",
                  nint,
                  ") for the desired burnin (",
                  burnin,
                  ")",
                  sep = ""),
            call. = FALSE)
    }
    startloc <- burnin + 1

    if (ll) {
      res <- sum(log(eloobject$winprobs[startloc:nint]))
    } else {
      res <- prod(eloobject$winprobs[startloc:nint])
    }
  }

  res
}
