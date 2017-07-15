#' create a dominance matrix
#'
#' create a dominance matrix from the underlying observed sequence
#'
#' @param eloobject output from \code{\link{elo.seq}}
#' @param daterange character of length 2, date range to which the matrix should correspond (default from beginning to end of sequence)
#' @param drawmethod character with the following options:\cr
#' \code{"omit"} = undecided interactions (draws/ties) are ignored (default)\cr
#' \code{"0.5"} = each undecided is counted half a win for each dyad member\cr
#' \code{"1"} = each undecided interaction is counted twice, i.e. as win for both individuals
#' @param onlyinteracting logical, indicating whether all individuals that were present (default, \code{TRUE}) are shown in the matrix, or only those that were involved in an interaction in the specified date period. If no presence data was supplied to \code{\link{elo.seq}}, it is assumed that all individuals were present at all times
#'
#' @return square matrix with dominance interactions (winner in rows, loser in columns)
#'
#' @author Christof Neumann
#'
#' @export
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#' # create dyadic matrix over the entire period of data collection
#' creatematrix(SEQ)
#' # limit to a subset of interactions
#' creatematrix(SEQ, daterange=c("2010-01-25", "2010-02-01"))
#' # limit to a subset of interactions and show only those IDs that were
#' # involved in at least one interaction
#' creatematrix(SEQ, daterange=c("2010-01-25", "2010-02-01"),
#'              onlyinteracting=TRUE)
#'
#' ## dealing with undecided interactions
#' data(adv2)
#' SEQ <- elo.seq(winner=adv2$winner, loser=adv2$loser, Date=adv2$Date,
#'                draw=adv2$tie)
#' # omit ties/draws
#' creatematrix(SEQ)
#' # omit ties/draws
#' creatematrix(SEQ, drawmethod="0.5")
#' # omit ties/draws
#' creatematrix(SEQ, drawmethod="1")

creatematrix <- function(eloobject, daterange=NULL, drawmethod="omit", onlyinteracting=FALSE) {
  # set the date range in case it's not specified...
  if(is.null(daterange[1])) {
    daterange <- c(min(eloobject$truedates), max(eloobject$truedates))
  } else {
    daterange <- as.Date(daterange)
    }

  # get the sequence
  dataseq <- eloobject$logtable
  dataseq$xdate <- eloobject$truedates[1] - 1 + dataseq$Date

  #restrict to date range
  dataseq <- dataseq[which(dataseq$xdate >= daterange[1] & dataseq$xdate <= daterange[2]), ]

  # create empty matrix based on presence
  pmat <- eloobject$pmat[which(eloobject$truedates == daterange[1]):which(eloobject$truedates == daterange[2]), ]
  IDS <- sort(colnames(pmat)[which(colSums(pmat) > 0)])

  mat <- matrix(ncol = length(IDS), nrow = length(IDS), 0)
  colnames(mat) <- rownames(mat) <- IDS
  mat1 <- mat

  # transform factors into characters...
  dataseq$winner <- as.character(dataseq$winner)
  dataseq$loser <- as.character(dataseq$loser)

  # add decided interactions
  xdata <- dataseq[dataseq$draw == FALSE, ]
  xdata <- table(xdata$winner, xdata$loser)
  mat[rownames(xdata), colnames(xdata)] <- xdata


  # add ties/draws, but separate depending on how they were specified to be treated (if present in the data)
  if(sum(dataseq$draw) > 0) {

    xdata <- dataseq[dataseq$draw == TRUE, ]
    xdata <- table(xdata$winner, xdata$loser)
    if(drawmethod == "0.5") {
      xdata <- xdata/2
      mat1[rownames(xdata), colnames(xdata)] <- xdata
      mat1 <- mat1 + t(mat1)
      mat <- mat + mat1
    }

    if(drawmethod == "1") {
      mat1[rownames(xdata), colnames(xdata)] <- xdata
      mat1 <- mat1 + t(mat1)
      mat <- mat + mat1
    }

  }

  # if "only interacting" was selected: remove those individuals from the matrix that havent interacted...
  if(onlyinteracting) {
    empty <- as.numeric(which(colSums(mat) + rowSums(mat) == 0))
    if(length(empty) > 0) mat <- mat[-empty, -empty]
  }

  return(mat)

}

