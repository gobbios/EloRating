#' prints its argument
#'
#' prints its argument
#'
#' @param x result from \code{\link{elo.seq}}
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'
#' @author Christof Neumann
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#' print(SEQ)

print.elo <- function(x, ...){

  cat("\nElo ratings from", x$misc["nID"], "individuals\n")
}
