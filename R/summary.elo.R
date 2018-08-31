#' summarize elo object
#'
#' summarize elo object
#'
#' @param object an object of class \code{"elo"}, usually the result of a call to \code{\link{elo.seq}}
#' @param ... further arguments passed to or from other methods (ignored)
#'
#' @author Christof Neumann
#'
#' @export
#'
#' @examples
#' IA <- randomsequence()
#' SEQ <- elo.seq(winner=IA$seqdat$winner, loser=IA$seqdat$loser,
#'                Date=IA$seqdat$Date, draw=IA$seqdat$Draw,
#'                presence=IA$pres)
#' summary(SEQ)

summary.elo <- function(object, ...) {
  cat("Elo ratings from", object$misc["nID"], "individuals\n")
  cat("total (mean/median) number of interactions: ", object$misc["nIA"], " (", object$misc["IAmean"], "/", object$misc["IAmedian"], ")", "\n", sep = "")
  cat("range of interactions:", object$misc["IAmin"], "-", object$misc["IAmax"], "\n")
  cat("date range:", object$misc["minDate"], "-", object$misc["maxDate"], "\n")
  cat("startvalue:", object$misc["startvalue"], "\n")
  cat("uppon arrival treatment:", object$misc["init"], "\n")
  cat("k:", object$misc["k"], "\n")
  cat("proportion of draws in the data set:", object$misc["draws"], "\n")
  cat("\n")
}
