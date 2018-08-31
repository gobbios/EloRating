#' hierarchy steepness based on David's scores
#' @aliases steepint
#'
#' @param mat square dominance matrix
#' @param nrand numeric, the number of runs for the randomization test
#' @param Dij logical, by default \code{TRUE} i.e. corrected for number of interactions in a dyad, otherwise simple proportion of wins/losses
#' @param returnfig logical, should a figure be produced that shows the distribution of expected steepness
#'
#' @return a named vector, with the observed steepness, the expected steepness, p-value and the number of randomizations used
#' @export
#'
#' @author Christof Neumann
#' @importFrom graphics abline
#'
#' @references
#' \insertRef{devries2006}{EloRating}
#' @examples
#' data(bonobos)
#' steepness(bonobos) # no randomization test
#'
#' # with randomization test
#' steepness(bonobos, nrand = 100)
#'

steepness <- function(mat, nrand = 0, Dij = TRUE, returnfig = FALSE) {
  temp <- steepint(mat, nrand = nrand, Dij = Dij)

  oristeep <- temp[1]
  if (nrand > 0) {
    randomsteeps <- temp[2:length(temp)]
    expectedsteep <- mean(randomsteeps)
    p <- sum(oristeep <= randomsteeps) / nrand
    if (p == 0) p <- 1 / nrand
    res <- c(oristeep, expectedsteep, p, nrand)
  } else {
    res <- c(oristeep, NA, NA, nrand)
  }

  if (returnfig & nrand > 0) {
    xt <- table(round(randomsteeps, 2))
    plot(xt, "h", xlim = c(0, 1), axes = FALSE, xlab = "steepness", ylab = "frequency")
    axis(1)
    axis(2, las = 1)
    abline(v = oristeep, col = "red", lwd = 2)
    legend(x = 0.5, y = max(xt) * 1.05, legend = c("expected", "observed"),
           col = c("black", "red"), lwd = 2, ncol = 2, xjust = 0.5,
           yjust = 0, xpd = TRUE)
  }

  names(res) <- c("steep", "expected", "p", "nrand")
  return(res)
}
