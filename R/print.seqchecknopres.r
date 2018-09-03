#' prints its argument
#'
#' prints its argument
#'
#' @param x result from \code{\link{seqcheck}}
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'
#' @author Christof Neumann
#'
#' @examples
#' data(adv)
#' print(seqcheck(winner = adv$winner, loser = adv$loser, Date = adv$Date))


print.seqchecknopres <- function(x, ...) {
  if (x$checksum["presence"] == 1) cat("No presence data supplied\n")

  if (x$checksum["seqdateorder"] == 1) cat("Date column is not ordered...WARNING\n")
  if (x$checksum["IDcheck"] == 1) cat("IDs occur in the data with inconsistent capitalization (ignore if on purpose)...WARNING\n")
  if (x$checksum["selfinteractions"] == 1) cat(x$selfinteractions, "\n", sep = "")
  if (x$checksum["length"] == 1) cat("Your data vectors do not match in length...ERROR\n")
  if (x$checksum["singledayobs"] == 1) cat("The following individuals were observed only on one day: ", paste(x$singledaycases, collapse = ", "), " ...WARNING\n", sep = "" )

  if (x$checksum["selfinteractions"] + x$checksum["IDcheck"] + x$checksum["length"] + x$checksum["singledayobs"] + x$checksum["seqdateorder"] == 0) cat("Everything seems to be fine with the interaction sequence...OK\n")
}
