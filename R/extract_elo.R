#' extract Elo ratings from elo object
#'
#' extract Elo ratings from elo object
#'
#' @param eloobject result from \code{\link{elo.seq}}
#' @param extractdate character, date on which Elo ratings should be obtained, defaults to the last day in the data set
#' @param standardize logical, should the returned ratings be scaled between 0 and 1. Default is \code{FALSE}. See \code{\link{scale_elo}}
#' @param IDs character, specify IDs for which ratings are returned. By default, returns all that were present on the date or at least on one day of the date range
#' @param NA.interpolate if \code{FALSE} (default), the last known rating is returned, which might not be from the specified date itself (but older). If \code{TRUE}, ratings on days without observations are linearly interpolated between days with known ratings (i.e. dates with observed interactions)
#' @param daterange if averaged ratings are desired, supply here the number of days from \cr
#' \code{extractdate - 1}. By default (\code{daterange = 1}), the ratings of the single \code{extractdate} are returned. \code{daterange = 2} produces average ratings from \code{extractdate} and the day after, and so on...
#' @details \code{extractdate} can be also a vector of dates. In this case, the \code{IDs} argument has to be either a vector of length 1 (i.e. a single individual) or a vector of the same length as \code{extractdate}. In the first case, the ratings for the same individual are returned on the dates specified in \code{extractdate}. In the second case, dates and IDs are matched, i.e. the rating of the individual on that date is returned in the same order as the dates/IDs vectors.
#' @return named (IDs) vector of (average) Elo ratings, or an unnamed vector of ratings (if length of \code{extracte} is larger than 1)
#'
#' @export
#'
#' @author Christof Neumann
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#' extract_elo(SEQ, "2010-01-30")
#' extract_elo(SEQ, "2010-01-30", standardize=TRUE)
#'
#' # same ratings (regardless of NA.interpolate),
#' # since "g" was observed on both days
#' extract_elo(SEQ, "2010-01-29", IDs="g")
#' extract_elo(SEQ, "2010-01-29", IDs="g", NA.interpolate=TRUE)
#'
#' extract_elo(SEQ, "2010-01-31", IDs="g")
#' extract_elo(SEQ, "2010-01-31", IDs="g", NA.interpolate=TRUE)
#'
#' # different ratings (depending on NA.interpolate),
#' # since "g" was not observed that day
#' extract_elo(SEQ, "2010-01-30", IDs="g")
#' extract_elo(SEQ, "2010-01-30", IDs="g", NA.interpolate=TRUE)
#'
#' extract_elo(SEQ, "2010-01-10", daterange=5)
#' extract_elo(SEQ, "2010-01-10", daterange=5, NA.interpolate=TRUE)
#'
#' # and for multiple dates and a single IDs
#' dates <- sample(adv$Date, size = 10, replace = TRUE)
#' ids <- "b"
#' extract_elo(eloobject = SEQ, extractdate = dates, standardize = FALSE, IDs = ids)
#'
#' # and for multiple dates and IDs
#' dates <- sample(adv$Date, size = 10, replace = TRUE)
#' ids <- sample(colnames(advpres)[2:8], size = 10, replace = TRUE)
#' extract_elo(eloobject = SEQ, extractdate = dates, standardize = FALSE, IDs = ids)
#'

extract_elo <- function(eloobject, extractdate = eloobject$misc["maxDate"],
                        standardize = FALSE, IDs = NULL, NA.interpolate = FALSE,
                        daterange = 1) {

  # for single extraction dates
  if (length(extractdate) == 1) {
    # which rating matrix to use
    ifelse(NA.interpolate == FALSE, mat <- eloobject$lmat, mat <- eloobject$cmat)

    # load presence matrix
    pmat <- eloobject$pmat

    # transform extraction into date and get all IDs present in the matrix
    edate <- as.Date(extractdate)
    allIDs <- colnames(mat)

    # if IDs are specified: check whether they are all in the matrix (if not: stop here)
    if (is.null(IDs) == FALSE) {
      for (i in IDs) {
        if (i %in% allIDs == FALSE) stop(i, " not among IDs\n")
        }
      }

    # create a date sequence and check whether extraction date lies within the range (if not: stop here)
    # since there is not date column in the rating matrix...
    DR <- seq(from = as.Date(eloobject$misc["minDate"]), to = as.Date(eloobject$misc["maxDate"]), by = "day")
    if (edate %in% DR == FALSE) stop("Date not in range", call. = FALSE)

    # transform date into numeric (i.e. the row number in the rating matrix),
    # and create a sequence (if daterange != 1),
    # stop if daterange goes beyond rating matrix
    edate <- which(DR == edate)
    edate <- edate:(edate + daterange - 1)
    if (max(edate) > nrow(mat)) {
      stop("specified daterange goes beyond dates in rating matrix", call. = FALSE)
    }

    # get the ratings of the specified date(range)
    x <- mat[edate, ]

    # standardize ratings (if wished)
    if (standardize) x <- scale_elo(x)

    # average ratings if daterange > 1
    if (daterange > 1) x <- colMeans(x, na.rm = TRUE)

    # replace NaNs (if present) by NAs
    if (length(which(is.nan(x))) > 0) x[which(is.nan(x))] <- NA

    # rounding
    ifelse(standardize, x <- round(x, 3), x <- round(x, 0))

    # restrict to present or specified IDs (in case of daterange > 1: if ID was present at least on one day it is taken into account)
    if (daterange == 1) {
      ifelse(is.null(IDs),
             x <- x[colnames(pmat)[which(pmat[edate, ] == 1)]],
             x <- x[IDs])
    }
    if (daterange > 1) {
      ifelse(is.null(IDs),
             x <- x[colnames(pmat)[which(colSums(pmat[edate, ], na.rm = TRUE) >= 1)]],
             x <- x[IDs])
    }

    # sort ratings
    return( sort(x, decreasing = TRUE, na.last = TRUE) )

  }

  # for multiple extraction dates
  if (length(extractdate) > 1) {
    if (is.null(IDs)) {
      stop("if more than one date is supplied either one ID has to be supplied, or as many as there are dates", call. = FALSE)
    }
    if (length(IDs) > 1) {
      if (length(IDs) != length(extractdate)) {
        stop("IDs vector and dates vector don't have the same length", call. = FALSE)
      }
    }

    # which rating matrix to use
    ifelse(NA.interpolate == FALSE, mat <- eloobject$lmat, mat <- eloobject$cmat)

    # standardize if required
    if (standardize) mat <- t(apply(mat, 1, scale_elo))

    # load presence matrix
    pmat <- eloobject$pmat

    # check that all dates are in date range
    alldates <- eloobject$truedates
    if (sum(!extractdate %in% alldates) != 0) stop("some dates are not in date range", call. = FALSE)

    # check that date range is different from its default
    if (daterange != 1) warning("daterange argument is ignored if multiple dates were supplied", call. = FALSE)

    # if there is only one ID, make an ID vector of same length as dates vector
    if (length(IDs) == 1) IDs <- rep(IDs, length(extractdate))

    # match dates with true dates
    daterows <- sapply(extractdate, function(X)which(alldates == X))

    # extract ratings
    x <- as.numeric(sapply(1:length(IDs), function(X)mat[daterows[X], IDs[X]]))
    return(x)
  }

}
