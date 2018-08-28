#' last day an individual was present
#'
#' last day an individual was present with respect to a reference date
#'
#' @param x output from \code{\link{elo.seq}}
#' @param ID character, if \code{"all"}, all individuals are returned, otherwise only for the desired ID
#' @param refdate character or Date (YYYY-MM-DD), up to which date the presence data should be considered, by default the last date of the sequene
#'
#' @return Date or \code{NA}
#'
#' @details the function can result in \code{NA} for two reasons. 1) the ID is not found in the presence data, which is accompanied by a warning and 2) the ID was not yet present if a referene date is specified
#'
#' @author Christof Neumann
#'
#' @export
#'
#' @examples
#' data(adv); data(advpres)
#' SEQ <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'                presence = advpres)
#' lastdaypresent(SEQ, ID = "all", refdate = "2010-01-02")
#' lastdaypresent(SEQ, ID = "f", refdate = "2010-02-02")


lastdaypresent <- function(x, ID = "all", refdate = NULL) {
  if (class(x) == "elo") {
    pm <- x$pmat
  } else {
    stop("so far 'x' must be of class 'elo'...", call. = FALSE)
  }
  if (is.null(refdate)) refdate <- max(x$truedates)

  pm <- pm[x$truedates <= refdate, ]

  # check for IDs that were never present and remove them from the presence data...
  # might happen if the refdate is early in the sequence and not yet all imigrants have arrived yet...
  if (0 %in% colSums(pm)) {
      pids <- names(pm)[colSums(pm) == 0]
      pm <- pm[, colSums(pm) > 0]
  } else {
    pids <- NA
  }

  res <- x$truedates[apply(pm, 2, function(z) max(which(z == 1)))]
  names(res) <- colnames(pm)

  # and add ids that were not present yet (if any)
  if (!is.na(pids[1])) {
    res <- c(res, rep(NA, length(pids)))
    names(res) <- c(names(pm), pids)
  }

  if (ID != "all") {
    if (ID %in% names(res)) {
      res <- as.Date(as.character(res[ID]))
    } else {
      res <- NA
      warning("ID not found...", call. = FALSE)
    }
  }

  return(res)
}
