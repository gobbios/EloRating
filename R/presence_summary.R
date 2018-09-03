#' Summarize presence data
#'
#' @param presence a data.frame with one date column (needs to be named "Date") and columns for each individual with 0/1 indicating absence/presence of that individual on that date
#' @param from character indicating the beginning of the period to be considered (by default the first date in the Date column)
#' @param to character indicating the end of the period to be considered (by default the last date in the Date column)
#'
#' @return a data.frame with entries for each individual indicating the first and last dates of their stays.
#' @export
#' @details
#' If an individual left and/or joined multiple times, this will be indicated by the \code{stint} column.
#'
#' The \code{init} column marks those individuals that were present on the beginning of the period considered.
#'
#' @examples
#' data(advpres)
#' presence_summary(advpres)
#'
#' presence_summary(advpres, from = "2010-01-27", to = "2010-02-02")

presence_summary <- function(presence, from = NULL, to = NULL) {
  dcol <- which(colnames(presence) == "Date")
  Date <- as.character(presence[, dcol])
  pmat <- as.matrix(presence[, -dcol])

  # first and last dates
  if (is.null(from)) {
    fdate <- Date[1]
  } else {
    fdate <- as.character(from)
  }
  if (is.null(to)) {
    ldate <- Date[length(Date)]
  } else {
    ldate <- as.character(to)
  }

  pmat <- pmat[which(as.Date(Date) >= as.Date(fdate) &
                       as.Date(Date) <= as.Date(ldate)), ]
  Date <- Date[which(as.Date(Date) >= as.Date(fdate) &
                       as.Date(Date) <= as.Date(ldate))]
  # exclude individuals that were never present
  cs <- colSums(pmat)
  ex <- names(cs)[cs == 0]

  res <- matrix(ncol = 4, nrow = 0, "")
  for (i in 1:ncol(pmat)) {
    x <- diff(pmat[, i])
    x <- sort(c(which(x == 1) + 1, which(x == -1)))

    if (length(x) == 0) {
      tempres <- c(colnames(pmat)[i], "1", fdate, ldate)
      res <- rbind(res, tempres)
    }
    if (length(x) > 0) {
      ds <- Date[x]
      # in case ID was present on the first date:
      if (pmat[1, i] == 1) ds <- c(fdate, ds)
      # in case ID was present on the last date:
      if (pmat[nrow(pmat), i] == 1) ds <- c(ds, ldate)

      dmat <- matrix(ds, ncol = 2, byrow = TRUE)
      tempres <- cbind(colnames(pmat)[i], 1:nrow(dmat), dmat)
      res <- rbind(res, tempres)
    }
  }
  res

  xres <- data.frame(ID = res[, 1], stint = as.numeric(res[, 2]),
                     from = as.Date(res[, 3]), to = as.Date(res[, 4]))
  xres$init <- xres$from == min(xres$from)
  xres$duration <- as.numeric(xres$to - xres$from) + 1

  # handle non-present IDs
  if (length(ex) > 0) {
    xres[xres$ID %in% ex, c("stint", "from", "to", "init", "duration")] <- NA
  }

  return(xres)
}
