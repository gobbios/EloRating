
#' standardize Elo ratings
#'
#' standardize Elo ratings between 0 and 1
#'
#' @param x numeric, a vector of Elo ratings
#'
#' @return a numeric vector of Elo ratings, which are scaled between 0 and 1, with the highest rating that is supplied becoming 1, the lowest becoming 0, and all others being proportionally scaled in between
#'
#' @export
#'
#' @aliases scale.elo
#'
#' @author Christof Neumann
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#' extract_elo(SEQ, "2010-01-30")
#' extract_elo(SEQ, "2010-01-30", standardize=TRUE)
#'
#' # same as
#' scale_elo(extract_elo(SEQ, "2010-01-30"))

scale_elo <- function (x) {
  round( (x - min(x, na.rm = TRUE) ) / max( (x - min(x, na.rm = TRUE) ), na.rm = TRUE), 3)
}
