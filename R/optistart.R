#' optimize start values
#'
#' experimental function to test different sets of randomly selected start
#' values
#'
#' @param eloobject output from \code{\link{elo.seq}}
#' @param burnin numeric, the number of interactions to be excluded from the
#'        calculation of the (log) likelihood. This parameter is ignored if a
#'        date range is supplied. By default \code{burnin = 0}, i.e. all
#'        interactions are considered.
#' @param spread numeric, the standard deviation of the ratings to be tested
#'        (by default 200)
#' @param runs numeric, number of inital ratings to be tested (by default 2000)
#' @param doplot logical, should the distribution of log likelihoods be plotted
#' @param initialcohort logical, not yet implemented
#'
#' @return a list with multiple items:
#' @author Christof Neumann
#' @details if the plot is produced, the red line indicates the log-likelihood
#'          when all individuals are assigned the same starting value
#'
#' the item \code{$best} reflects the optimal start values found
#' @export
#'
#' @examples
#' set.seed(123)
#' xdata <- randomsequence(8, 100)$seqdat
#' res1 <- elo.seq(xdata$winner, xdata$loser, xdata$Date)
#' ores <- optistart(res1)
#' res2 <- elo.seq(xdata$winner, xdata$loser, xdata$Date, startvalue = ores$best)
#' eloplot(res1)
#' eloplot(res2)

optistart <- function(eloobject,
                      burnin = 0,
                      spread = 200,
                      runs = 2000,
                      doplot = FALSE,
                      initialcohort = TRUE) {
  allids <- eloobject$allids
  n <- length(allids)
  normprob <- eloobject$misc["normprob"] == "1"

  w <- eloobject$logtable$winner
  l <- eloobject$logtable$loser
  kval <- eloobject$kvals
  startval <- eloobject$startvalues

  resmat <- matrix(ncol = length(allids), nrow = runs)
  colnames(resmat) <- allids
  logliks <- numeric(runs)
  for(i in 1:nrow(resmat)) {
    svals <- rnorm(n, mean = startval, sd = spread)
    svals <- round(svals - mean(svals) + startval)
    logliks[i] <- likelo(fastelo(WINNER = w,
                                 LOSER = l,
                                 ALLIDS = allids,
                                 KVALS = kval,
                                 STARTVALUES = svals,
                                 NORMPROB = normprob),
                         burnin = burnin)
    resmat[i, ] <- svals
  }
  original <- likelo(fastelo(WINNER = w,
                             LOSER = l,
                             ALLIDS = allids,
                             KVALS = kval,
                             STARTVALUES = rep(startval, n),
                             NORMPROB = normprob))
  if (doplot) {
    x <- hist(logliks, breaks = 50, plot = FALSE)
    plot(x, ylim = c(0, max(x$counts)*1.05), yaxs = "i", las = 1, main = "")
    box()
    abline(v = original, col = "red", lwd = 2)
  }

  list(best = resmat[which.max(logliks), ],
       val = max(logliks),
       original = original,
       resmat = resmat,
       logliks = logliks)
}
