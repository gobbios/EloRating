#' optimize the k parameter
#'
#' @param eloobject output from \code{\link{elo.seq}} or from \code{\link{fastelo}}
#' @param krange either a vector of length 2, giving the range of k values to be tested, or a named list with vectors of length 2, in which each list item is named according to different interaction types (see the \code{intensity=} argument in \code{\link{elo.seq}})
#' @param optimode character, either \code{"loop"} or \code{"optimize"}. See details.
#' @param resolution numeric, the number of steps between the range of k values to be tested. Currently only a single value can be supplied here and in case \code{krange} is a list this value will be applied to all items in this list
#' @param itype character or factor containing the different interaction types, which is only relevant if \code{krange} is a list. The content of \code{itype} and the names of \code{krange} have to match!
#' @param daterange character or Date of length 2, provides a date range for optimization. Only relevant in case \code{eloobject} is the result of \code{elo.seq()}
#' @param burnin numeric, the number of interactions to be excluded from the calculation of the (log) likelihood. This parameter is ignored if a date range is supplied. By default \code{burnin = 0}, i.e. all interactions are considered.
#' @param doplot logical, should a plot be returned. Works only if \code{optimode = "loop"}, and only if there are maximally two different interaction types
#' @param progbar logical, should a progress bar be displayed, not yet implemented
#' @param ... additional arguments for the plot and text functions, e.g. for setting \code{cex} or \code{lwd}
#' @details this function attempts to find the objectively best k parameter. This is done by a maximum likelihood approach in which the likelihood is represented by the individual winning probabilities. In a perfect situation, in each interaction the winner would have a winning probability of 1, whereas in the worst case, in each interaction the winner would have a winning probability of 0.
#'
#' There are two major approaches to find the best k. One does it 'by hand', i.e. by means of a loop trying many different k values (specified by \code{resolution}), recalculating the ratings (and associated winning probabilities) and return the likelihood for each k value. The second approach uses the \code{optimize} function, but this is not yet implemented.
#'
#' One thing to note is that you can use interaction-level k values, i.e. if you have interactions of different types (e.g. fights vs. displacements) you can try to find the optimal k for each interaction type. This is achieved in the (\code{"loop"} approach by trying different \emph{combinations} of k values. Because of the combinatorial nature of this approach, the number of individual sequences to be fitted increases sharply with higher resolutions: if you have two different interaction types and use a resolution of 5, the function will need to run 25 (= 5 * 5) iterations. If you use a more reasonable resolution of 100 the number of iterations will be already 10000. Also note that in that case the actual plotting of the results might take a lot of time in such cases. Just try with low values first to see whether it works as expected and the potentially increase the resolution.
#' @return a list with two items: (1) \code{$best}, a data frame with one line, in which the maximal log likelihood is returned alongside the one or several corresponding k values, and (2) \code{$complete}, a data frame with all the values tested and their log likelihoods
#' @export
#' @importFrom graphics text
#' @importFrom stats as.formula
#' @references
#' \insertRef{franz2015a}{EloRating}
#'
#' \insertRef{mcmahan1984}{EloRating}
#' @examples
#' data(adv2)
#' res <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date)
#' optimizek(eloobject = res, krange = c(50, 400), resolution = 200, doplot = TRUE)$best
#'
#' # with a burnin value set:
#' optimizek(eloobject = res, krange = c(50, 400), resolution = 200, burnin = 15, doplot = TRUE)$best
#'
#' # using different interaction intensities
#' myks <- list(displace = 20, fight = 200)
#' res <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date,
#'                k = myks, intensity = adv2$intensity)
#' optimizek(eloobject = res, optimode = "loop",
#'           krange = list(fight = c(50, 600), displace = c(20, 200)),
#'           resolution = 100, itype = adv2$intensity, main = 'bla')$best


optimizek <- function(eloobject, krange = c(2, 400), optimode = "loop",
                      resolution = 100, itype = NULL, daterange = NULL,
                      burnin = 0, doplot = FALSE, progbar = FALSE, ...) {
  # recreate interaction data
  if (eloobject$rtype == "elo.seq") {
    winner <- eloobject$logtable$winner
    loser <- eloobject$logtable$loser
    Date <- eloobject$truedates[eloobject$logtable$Date]
    startvals <- eloobject$startvalues
    if (is.list(krange)) interactiontype <- eloobject$logtable$intensity
    # adjust according to desired date range
    if (!is.null(daterange)) {
      x <- which(eloobject$truedates[eloobject$logtable$Date] >= as.Date(daterange[1]) &
                   eloobject$truedates[eloobject$logtable$Date] <= as.Date(daterange[2]))
      winner <- winner[x]
      loser <- loser[x]
      if (is.list(krange)) interactiontype <- interactiontype[x]
      # Date <- Date[x]
      # draw <- draw[x]
    }
    # general info about how the original sequence was fitted
    normprob <- eloobject$misc["normprob"] == "1"
    allids <- eloobject$allids
    xround <- TRUE
  }

  if (eloobject$rtype == "fastelo") {
    winner <- eloobject$winner
    loser <- eloobject$loser
    startvals <- eloobject$startvalues
    allids <- eloobject$allids
    normprob <- eloobject$normprob
    # handle intensity vector
    if (is.list(krange)) {
      interactiontype <- itype
      if(is.null(itype)) stop("no interaction types provides (argument 'itype')")
      if (!identical(sort(names(krange)), sort(unique(itype)))) stop("data in 'itype' and 'klist' don't match")
    }
    xround <- eloobject$round
  }

  if (optimode == "loop") {

    if (is.list(krange)) {
      x <- lapply(krange, function(X) seq(from = X[1], to = X[2], length.out = resolution))
      res <- expand.grid(x)
      ll <- numeric(nrow(res))

      for (i in 1:nrow(res)) {
        testks <- as.list(as.numeric(res[i, ]))
        names(testks) <- colnames(res)
        ks <- unlist(testks[interactiontype])
        tempres <- fastelo(WINNER = winner, LOSER = loser, ALLIDS = allids,
                           KVALS = ks, STARTVALUES = startvals,
                           NORMPROB = normprob, ROUND = xround)
        ll[i] <- likelo(tempres, burnin = burnin)
      }
      res$loglik <- ll

    } else {
      res <- data.frame(k = seq(from = krange[1], to = krange[2], length.out = resolution))
      ll <- numeric(nrow(res))
      for (i in 1:nrow(res)) {
        tempres <- fastelo(WINNER = winner, LOSER = loser, ALLIDS = allids,
                           KVALS = rep(res$k[i], length(winner)),
                           STARTVALUES = startvals, NORMPROB = normprob, ROUND = xround)
        ll[i] <- likelo(tempres, burnin = burnin)
      }
      res$loglik <- ll
    }

    if (doplot) {
      if (!is.list(krange)) {
        plot(res$k, res$loglik, type = "l", las = 1, xlab = bquote(italic(k)),
             ylab = "log likelihood", cex.axis = 0.8)
        abline(v = res$k[which.max(res$loglik)], col = "red")
      } else {
        if (length(krange) == 2) {
          f <- as.formula(paste("loglik ~ ", names(res)[2], "+", names(res)[1]))
          # heatmapplot(loglik ~ displace + fight, data = res, ...)
          heatmapplot(formula = f, data = res, ...)
        } else {
          message("more than 2 intensities: no figure produced")
        }
      }

    }
  }

  if (optimode == "optimize") {
    message("not yet implemented. Use 'optimode = 'loop'' instead")
  }

  return( list(best = res[which.max(res$loglik), ], complete = res ))
}

