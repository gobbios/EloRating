#' calculate dominance trajectory
#'
#' calculate individual Elo rating trajectory over time
#'
#' @param eloobject result from \code{\link{elo.seq}}
#' @param ID character, the ID(s) of the individual(s)
#' @param from character, from which date onwards should the trajectory be calculated. By default the first date in the sequence is used
#' @param to character, until which date should the trajectory be calculated. By default the last date in the sequence is used
#'
#' @return A \code{data.frame} with as many lines as specified in \code{ID}, columns for ID, date range, the actual slope (trajectory), and the number of observed interactions within the date range
#'
#' @author Christof Neumann
#'
#' @importFrom stats lm
#'
#' @export
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' traj_elo(SEQ, "a")
#'
#' traj_elo(SEQ, "a", from = "2010-01-20", to = "2010-01-30")
#'
#' # no slope available if ID was not observed interacting
#' # inside the date range
#' traj_elo(SEQ, "a", from = "2010-01-17", to = "2010-01-18")
#'
#' # no slope available if ID was only observed interacting
#' # once within the date range
#' traj_elo(SEQ, "a", from = "2010-01-17", to = "2010-01-19")
#'
#' # for several individuals
#' traj_elo(SEQ, c("a", "b", "c"))

traj_elo <- function(eloobject, ID, from = min(eloobject$stability$date), to = max(eloobject$stability$date)){

  # check integrity of dates
  if (as.Date(to) < as.Date(from)) stop("'from' date is later than 'to' date", call. = FALSE)

  # get lines that correspond to date range
  DR <- seq(from = as.Date(eloobject$misc["minDate"]), to = as.Date(eloobject$misc["maxDate"]), by = "day")
  if ( (as.Date(from) %in% DR & as.Date(to) %in% DR) == FALSE ) stop("one of the dates is out of date range", call. = FALSE)
  DR <- which(DR == as.Date(from)) : which(DR == as.Date(to))

  # check whether IDs are among individuals
  excl <- NULL
  for (i in ID) {
    if (!i %in% eloobject$allids) excl <- c(excl, i)
  }

  if (length(excl) > 0) warning(paste0("the following IDs do not occur in the data: ", paste(excl, collapse = ", ")), call. = FALSE)

  # create output object
  res <- data.frame(ID = ID, fromDate = as.Date(from), toDate = as.Date(to), slope = NA, Nobs = NA)

  for (i in 1:length(ID)) {
    if (ID[i] %in% eloobject$allids) {
      # extract ratings for ID during the date range
      traj <- eloobject$mat[DR, ID[i]]

      # how many data points
      res$Nobs[i] <- length(na.omit(traj))

      # calculate slope (but only if there were actual observations...)
      if (res$Nobs[i]  > 1 ) res$slope[i] <- as.numeric(lm(traj ~ DR)$coefficients["DR"])
      if (res$Nobs[i] <= 1 ) message(paste("no (or only one) observation for", ID[i], "during specified date range\n"))
    }
  }

  return(res)
}
