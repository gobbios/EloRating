


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
#'
#' @return named (IDs) vector of (average) Elo ratings
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


extract_elo <- function(eloobject, extractdate=eloobject$misc["maxDate"],  standardize=FALSE, IDs=NULL, NA.interpolate=FALSE, daterange=1) {

  # which rating matrix to use
  ifelse(NA.interpolate==FALSE, mat <- eloobject$lmat, mat <- eloobject$cmat)

  # load presence matrix
  pmat <- eloobject$pmat

  # transform extraction into date and get all IDs present in the matrix
  edate <- as.Date(extractdate); allIDs <- colnames(mat)

  # if IDs are specified: check whether they are all in the matrix (if not: stop here)
  if(is.null(IDs)==FALSE) {for(i in IDs) { if(i %in% allIDs == FALSE) stop(i, " not among IDs\n") } }

  # create a date sequence and check whether extraction date lies within the range (if not: stop here)
  # since there is not date column in the rating matrix...
  DR <- seq(from=as.Date(eloobject$misc["minDate"]), to=as.Date(eloobject$misc["maxDate"]), by="day")
  if(edate %in% DR == FALSE) stop("Date not in range")

  # transform date into numeric (i.e. the row number in the rating matrix), and create a sequence (if daterange != 1), stop if daterange goes beyond rating matrix
  edate <- which(DR == edate); edate <- edate:(edate+daterange-1); if(max(edate) > nrow(mat)) stop("specified daterange goes beyond dates in rating matrix")

  # get the ratings of the specified date(range)
  x <- mat[edate, ]

  # standardize ratings (if wished)
  if(standardize) x <- scale_elo(x)

  # average ratings if daterange > 1
  if(daterange>1) x <- colMeans(x, na.rm=TRUE)

  # replace NaNs (if present) by NAs
  if(length(which(is.nan(x)))>0) x[which(is.nan(x))] <- NA

  # rounding
  ifelse(standardize, x <- round(x, 3), x <- round(x,0))

  # restrict to present or specified IDs (in case of daterange > 1: if ID was present at least on one day it is taken into account)
  if(daterange==1) {
    ifelse(is.null(IDs),
           x <- x[ colnames(pmat)[ which(pmat[edate, ]==1) ] ],
           x <- x[ IDs ])
  }
  if(daterange>1) {
    ifelse(is.null(IDs),
           x <- x[ colnames(pmat)[ which(colSums(pmat[edate, ], na.rm=TRUE)>=1)] ],
           x <- x[ IDs ])
  }

  # sort ratings
  return(sort(x, decreasing = TRUE, na.last = TRUE))

}
