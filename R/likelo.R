#' (log) likelihood of Elo-rating model
#'
#' @param eloobject output from \code{\link{elo.seq}}
#' @param daterange character or Date of length 2, gives the date range for which likelihood should be calculated. By default, the entire date range is considered.
#' @param ll logical, should the log likelihood be returned rather than the likelihood, by default \code{TRUE}
#' @param burnin not yet implemented
#' @details This function returns the (log) likelihood of a dominance interaction sequence. The likelihood is the product of all winning probabilities (for each interaction).
#' @return numeric of length 1, the log likelihood
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

likelo <- function(eloobject, daterange = NULL, burnin = NULL, ll = TRUE) {
  if (class(eloobject) == "elo") {
    temp <- eloobject$logtable
    normprob <- eloobject$misc["normprob"] == "1"
    if (!is.null(daterange)) {
      d1 <- which(eloobject$truedates == as.Date(daterange[1]))
      d2 <- which(eloobject$truedates == as.Date(daterange[2]))
      temp <- temp[temp$Date >= d1 & temp$Date <= d2, ]
    }
    foo <- function(X) winprob(elo1 = X[1], elo2 = X[2], normprob = normprob)
    winprobs <- apply(temp[, c("Apre", "Bpre")], MARGIN = 1, FUN = foo)
    if (ll) res <- sum(log(winprobs)) else res <- prod(winprobs)
  }

  if (class(eloobject) == "list") {
    if (!is.null(daterange)) message("date range ignored for results from fastelo")
    if (ll) res <- sum(log(eloobject[[2]])) else res <- prod(eloobject[[2]])
  }
  return(res)
}
