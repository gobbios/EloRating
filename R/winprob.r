#' expected winning probability
#'
#' calculate expected probability of winning given known strengths of two opponents
#'
#' @param elo1 Elo rating from individual for which the winning probability should be calculated
#' @param elo2 Elo rating of the opponent
#' @param normprob logical (by default \code{TRUE}). Should a normal curve be assumed for calculating the winning/losing probablities, or not (see details).
#' @param fac numeric (by default \code{NULL}). A scaling factor (see details)
#'
#'
#' @details \insertCite{elo1978;textual}{EloRating} proposed three ways of calculating winning probabilities (section 8.73), one of which (the `linear' approach) is ignored here because it ``lacks the sophistication and flexibility to express the limitation on D [rating difference] and the deflation controls required for integrity of the ratings''. Between the two remaining approaches (normal and logistic), Elo favored initially the normal over the logistic function, though he writes that the logistic function ``better reflects large deviations in an extended series''. Because of Elo's initial preference, the default approach taken by the package's functions is the normal one, though it can be changed to the logistic one if desired.
#'
#' In the meantime, several studies have used an addtional approach to calculate winning probabilities, which is based on an exponential distribution. This can be invoked by setting \code{normprob = FALSE} and \code{fac} to some number. The value I have seen used is 0.01 \insertCite{franz2015a}{EloRating}. \insertCite{sanchez-tojar2018;textual}{EloRating} refer to it as \code{sigmoid.param} in their \code{aniDom} package. \insertCite{goffe2018;textual}{EloRating} also use this approach but their scaling factor is 1 (referred to as \code{diff_f}) because their ratings are on a completely different scale.
#'
#' Finally, this function is for demonstration only, i.e. it is not used anywhere in the package (other than in vignettes). As such, the functions in the package (most importantly \code{\link{e.single}}) only allow the two primary options for the calculation of winning probabilities (for now).
#'
#' @return numeric, expected chance of first individual to win an interacation with the second individual
#'
#' @references
#' \insertRef{elo1978}{EloRating}
#'
#' \insertRef{franz2015a}{EloRating}
#'
#' \insertRef{sanchez-tojar2018}{EloRating}
#'
#' \insertRef{goffe2018}{EloRating}
#'
#' @author Christof Neumann
#'
#' @importFrom stats pnorm
#'
#' @examples
#' winprob(1200,1000)
#' winprob(1000,1200)
#' winprob(1000,1000)
#' winprob(1200,1000, normprob = FALSE)
#' winprob(1000,1200, normprob = FALSE)
#' winprob(1000,1000, normprob = FALSE)
#' winprob(1200,1000, normprob = FALSE, fac = 0.01)
#' winprob(1000,1200, normprob = FALSE, fac = 0.01)
#' winprob(1000,1000, normprob = FALSE, fac = 0.01)
#'
#' # compare different algorithms visually
#' w <- rep(0, 1001) # winner rating: constant
#' l <- w - 0:1000 # loser rating: varying
#'
#' elonorm <- numeric(length(w))
#' eloexpo <- numeric(length(w))
#' eloopti <- numeric(length(w))
#' eloopti2 <- numeric(length(w))
#'
#' for(i in 1:length(w)) {
#'   elonorm[i] <- winprob(w[i], l[i], normprob = TRUE)
#'   eloexpo[i] <- winprob(w[i], l[i], normprob = FALSE)
#'   eloopti[i] <- winprob(w[i], l[i], normprob = FALSE, fac = 0.01)
#'   eloopti2[i] <- winprob(w[i], l[i], normprob = FALSE, fac = 0.005)
#' }
#'
#' plot(0, 0, type = "n", las = 1, yaxs = "i",
#'      xlim = c(0, 1000), ylim = c(0.5, 1),
#'      xlab = "rating difference",
#'      ylab = "winning probability")
#' points(abs(l), elonorm, "l", col = "#4B0055", lwd = 3)
#' points(abs(l), eloexpo, "l", col = "#007094", lwd = 3)
#' points(abs(l), eloopti, "l", col = "#00BE7D", lwd = 2)
#' points(abs(l), eloopti2, "l", col = "#FDE333", lwd = 2)
#'
#' legend("bottomright",
#'        legend = c("normal", "logistic", "exponential (fac = 0.01)", "exponential (fac = 0.005)"),
#'        col = c("#4B0055", "#007094", "#00BE7D", "#FDE333"),
#'        lwd = 2,
#'        cex = 0.9)
#'
#' @export


winprob <- function(elo1,
                    elo2,
                    normprob = TRUE,
                    fac = NULL) {

  if (normprob) {
    # z score based on fixed SD=200 (see Elo 1978)
    z_score <- (elo1 - elo2) / (200 * sqrt(2))
    p_win <- pnorm(z_score)
  } else {
    if (is.null(fac)) {
      p_win <- 1 - 1/(1 + 10 ^ ((elo1 - elo2) / 400))
    } else {
      p_win <- 1 / (1 + exp(fac * (elo2 - elo1)))
      # Goffe's: p_win <- 1/(1 + exp(diff_f * (elo2 - elo1)))
    }
  }

  return(p_win)
}
