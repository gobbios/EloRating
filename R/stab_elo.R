#' stability index \emph{S}
#'
#' calculates the \emph{S} index as metric for the overall stability of a hierarchy during a specified time period
#'
#' @param eloobject an object of class \code{"elo"}, usually the result of a call to \code{\link{elo.seq}}
#' @param from character, from which date onwards should \emph{S} be calculated. By default the first date in the sequence is used
#' @param to character, until which date should \emph{S} be calculated. By default the last date in the sequence is used
#' @param weight logical, should single rank changes be weighted by the Elo rating of the highest-rated individual involved in a rank change? Default is \code{TRUE}
#'
#' @return returns the \emph{S} index
#' @export
#'
#' @aliases stab.elo
#'
#' @author Christof Neumann
#'
#' @details \emph{S} ranges between 0 and 1, where 0 indicates an unstable hierarchy, in which the ordering reverses every other day, and 1, in which the ordering is stable and no rank changes occur.
#'
#' In contrast to the originally proposed \emph{S}, this version is now standardized between 0 and 1, and additionally, the interpretation is reversed, i.e. 1 refers to stable situations, whereas values closer to 0 indicate more instable hierarchies
#'
#' @references
#' \insertRef{neumann2011}{EloRating}
#'
#' \insertRef{mcdonald2013a}{EloRating}
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#' stab_elo(SEQ)
#' stab_elo(SEQ, weight=FALSE)
#' stab_elo(SEQ, from="2010-01-20", to="2010-01-30")
#' stab_elo(SEQ, from="2010-01-20", to="2010-01-30", weight=FALSE)


stab_elo <- function(eloobject, from=min(eloobject$stability$date), to=max(eloobject$stability$date), weight=TRUE) {

  # create errors if the date range lies outside the supplied data
  if (as.Date(from) < min(eloobject$stability$date) | as.Date(from) > max(eloobject$stability$date)) stop("start date outside date range")
  if (as.Date(to) < min(eloobject$stability$date) | as.Date(to) > max(eloobject$stability$date)) stop(paste(to, "outside date range"))

  # subset the stability dataframe in the eloobject according to the specified dates
  stab <- subset(eloobject$stability, date >= as.Date(from) & date <= as.Date(to))

  # calculate and return stability either with (default) or without the weighing factor
  if (weight == TRUE) {
    S <- round(sum(stab$rankdiffs * stab$eloweights) / (sum(floor(stab$Idspresent ^ 2 / 2))), 4)
    return(1 - S)
  } else {
    S <- round(sum(stab$rankdiffs) / (sum(floor(stab$Idspresent ^ 2 / 2))), 4)
    return(1 - S)
  }
}
