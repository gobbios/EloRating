#' optimize the k parameter
#'
#' @param eloobject output from \code{\link{elo.seq}}
#' @param krange either a vector of length 2, giving the range of k values to be tested, or a named list with vectors of length 2, in which each list item is named according to different interaction types (see the \code{intensity=} argument in \code{\link{elo.seq}})
#' @param optimode character, either \code{"loop"}, \code{"loopfast"} or \code{"optimize"}
#' @param resolution numeric, the number of steps between the range of k values to be tested. Currently only a single value can be supplied here and in case \code{krange} is a list this value will be applied to all items in this list
#' @param daterange character or Date of length 2, provides a date range for optimization
#' @param doplot logical, should a plot be returned. Works only in the \code{optimode = "loop"} and \code{optimode = "loopfast"} and only if there are maximally two different interaction types
#' @param progbar logical, should a progress bar be displayed, not yet implemented
#' @param ... additional arguments for the plot and text functions, e.g. for setting \code{cex} or \code{lwd}
#' @details this function attempts to find the objectively best k parameter. This is done by a maximum likelihood approach in which the likelihood is represented by the individual winning probabilities. In a perfect situation, in each interaction the winner would have a winning probability of 1, whereas in the worst case, in each interaction the winner would have a winning probability of 0.
#'
#' There are two major approaches to find the best k. One does it 'by hand', i.e. by means of a loop trying many different k values (specified by \code{resolution}), recalculating the ratings (and associated winning probabilities) and return the likelihood for each k value. The second approach uses the \code{optimize} function, but this is not yet implemented. For the loop approach, there are two implementations. The first uses internally the \code{elo.seq()} function. This approach is relatively slow and the only advantage is that it can account for tied interaction outcomes. The second uses a much faster function (\code{fastelo()}) that just does the bare minimum in terms of rating calculations (and cannot yet handle ties).
#'
#' One thing to note is that you can use interaction-level k values, i.e. if you have interactions of different types (e.g. fights vs. displacements) you can try to find the optimal k for each interaction type. This is achieved in the loop approaches (\code{"loop"} and \code{"loopfast"}) by trying different \emph{combinations} of k values. Because of the combinatorial nature of this approach, the number of individual sequences to be fitted increases sharply with higher resolutions: if you have two different interaction types and use a resolution of 5, the function will need to run 25 (= 5 * 5) iterations. If you use a more reasonable resolution of 100 the number of iterations will be already 10000. Also note that in that case the actual plotting of the results might take a lot of time. Just try with low values first to see whether it works as expected and the potentially increase the resolution.
#' @return a data frame with one line, in which the maximal log likelihood is returned alongside the one or several corresponding k values
#' @export
#' @importFrom graphics text
#' @references
#' \insertRef{franz2015a}{EloRating}
#'
#' \insertRef{mcmahan1984}{EloRating}
#' @examples
#' data(adv2)
#' res <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date)
#' optimizek(eloobject = res, krange = c(50, 400), resolution = 200, doplot = TRUE)
#'
#' # using different interaction intensities
#' myks <- list(displace = 20, fight = 200)
#' res <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date,
#'                k = myks, intensity = adv2$intensity)
#' optimizek(eloobject = res, optimode = "loopfast",
#'           krange = list(fight = c(50, 600), displace = c(20, 200)),
#'           resolution = 20, doplot = TRUE)
#'
#' # optimode = "loopfast" is much faster
#' \dontrun{
#' data("baboons2")
#' xdata <- baboons2
#' res <- elo.seq(winner = as.character(xdata$Winner), loser = as.character(xdata$Loser),
#'                Date = as.character(xdata$Date))
#' # slow
#' optimizek(res, optimode = "loop", doplot = TRUE, resolution = 100, krange = c(50, 400))
#' # fast (despite higher resolution)
#' optimizek(res, optimode = "loopfast", doplot = TRUE, resolution = 1000, krange = c(50, 400))}


optimizek <- function(eloobject, krange = c(2, 400), optimode = "loop",
                      resolution = 100, daterange = NULL, doplot = FALSE,
                      progbar = FALSE, ...) {
  # recreate interaction data
  winner <- eloobject$logtable$winner
  loser <- eloobject$logtable$loser
  Date <- eloobject$truedates[eloobject$logtable$Date]
  draw <- eloobject$logtable$draw

  # adjust according to desired date range
  if (!is.null(daterange)) {
    x <- which(eloobject$truedates[eloobject$logtable$Date] >= as.Date(daterange[1]) &
               eloobject$truedates[eloobject$logtable$Date] <= as.Date(daterange[2]))
    winner <- winner[x]
    loser <- loser[x]
    Date <- Date[x]
    draw <- draw[x]
  }

  # general info about how the original sequence was fitted
  normprob <- eloobject$misc["normprob"] == "1"

  if (optimode == "loop") {
    if (is.list(krange)) {
      intensity <- eloobject$logtable$intensity
      if (!is.null(daterange)) intensity <- intensity[x]
      x <- lapply(krange, function(X) seq(from = X[1], to = X[2], length.out = resolution))
      res <- expand.grid(x)
      ll <- numeric(nrow(res))

      for (i in 1:nrow(res)) {
        testks <- as.list(as.numeric(res[i, ]))
        names(testks) <- colnames(res)
        tempres <- elo.seq(winner = winner, loser = loser, Date = Date,
                           draw = draw, normprob = normprob,
                           intensity = intensity,
                           runcheck = FALSE, progressbar = FALSE,
                           k = testks)
        ll[i] <- likelo(tempres)
      }
      res$loglik <- ll
      if (length(krange) == 2 & doplot) {
        plot(0, 0, type = "n", xlab = colnames(res)[1], ylab = colnames(res)[2],
             ylim = range(res[, 2]), xlim = range(res[, 1]), las = 1, ...)
        pdata <- res[-which.max(res$loglik), ]
        text(x = pdata[, 1],
             y = pdata[, 2],
             labels = round(res$loglik, 2), ...)
        text(x = res[which.max(res$loglik), 1],
             y = res[which.max(res$loglik), 2],
             labels = round(res[which.max(res$loglik), 3], 2),
             col = "red", font = 2, ...)
      }
      if (length(krange) > 2 & doplot) {
        message("more than 2 intensities: no figure produced")
      }
    } else {
      res <- data.frame(k = seq(from = krange[1], to = krange[2], length.out = resolution))
      ll <- numeric(nrow(res))
      for (i in 1:nrow(res)) {
        tempres <- elo.seq(winner = winner, loser = loser, Date = Date,
                           draw = draw, normprob = normprob,
                           runcheck = FALSE, progressbar = FALSE,
                           k = res$k[i])
        ll[i] <- likelo(tempres)
      }
      res$loglik <- ll
      if (doplot) {
        plot(res$k, res$loglik, type = "l", las = 1, xlab = bquote(italic(k)),
             ylab = "log likelihood", ...)
        abline(v = res$k[which.max(res$loglik)], col = "red", ...)
      }
    }
  }

  if (optimode == "loopfast") {
    # objects specific to fastelo
    allids <- eloobject$allids
    startvals <- rep(1000, length(allids))

    if (is.list(krange)) {
      intensity <- eloobject$logtable$intensity
      if (!is.null(daterange)) intensity <- intensity[x]
      x <- lapply(krange, function(X) seq(from = X[1], to = X[2], length.out = resolution))
      res <- expand.grid(x)
      ll <- numeric(nrow(res))

      for (i in 1:nrow(res)) {
        testks <- as.list(as.numeric(res[i, ]))
        names(testks) <- colnames(res)
        ks <- unlist(testks[intensity])
        tempres <- fastelo(WINNER = winner, LOSER = loser, ALLIDS = allids,
                           KVALS = ks, STARTVALUES = startvals,
                           NORMPROB = normprob)
        ll[i] <- likelo(tempres)
      }
      res$loglik <- ll
      if (length(krange) == 2 & doplot) {
        plot(0, 0, type = "n", xlab = colnames(res)[1], ylab = colnames(res)[2],
             ylim = range(res[, 2]), xlim = range(res[, 1]), las = 1, ...)
        pdata <- res[-which.max(res$loglik), ]
        text(x = pdata[, 1],
             y = pdata[, 2],
             labels = round(res$loglik, 2), ...)
        text(x = res[which.max(res$loglik), 1],
             y = res[which.max(res$loglik), 2],
             labels = round(res[which.max(res$loglik), 3], 2),
             col = "red", font = 2, ...)
      }
      if (length(krange) > 2 & doplot) {
        message("more than 2 intensities: no figure produced")
      }
    } else {
      res <- data.frame(k = seq(from = krange[1], to = krange[2], length.out = resolution))
      ll <- numeric(nrow(res))
      for (i in 1:nrow(res)) {
        tempres <- fastelo(WINNER = winner, LOSER = loser, ALLIDS = allids,
                           KVALS = rep(res$k[i], length(winner)),
                           STARTVALUES = startvals, NORMPROB = normprob)
        ll[i] <- likelo(tempres)
      }
      res$loglik <- ll
      if (doplot) {
        plot(res$k, res$loglik, type = "l", las = 1, xlab = bquote(italic(k)),
             ylab = "log likelihood", ...)
        abline(v = res$k[which.max(res$loglik)], col = "red", ...)
      }
    }
  }


  if (optimode == "optimize") {

  }

  return( res[which.max(res$loglik), ] )
}
