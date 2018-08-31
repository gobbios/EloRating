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
#' @param winners vector of winners (see details)
#' @param losers vector of losers (see details)
#' @param draw logical vector (currently not doing anything)
#'
#' @return square matrix with dominance interactions (winner in rows, loser in columns)
#'
#' @author Christof Neumann
#'
#' @details The function works with either the output of \code{\link{elo.seq}}, or with two vectors of winners and losers. If you use winner and loser vectors, their arguments need to be named, and also the remaining arguments (\code{daterange=} and \code{onlyinteracting=}) are ignored. The function does not yet allow to include draws if you supply winner/loser vectors. If you go via the \code{\link{elo.seq}}-route, the function can handle draws (via the \code{drawmethod=} argument).
#' @export
#'
#' @examples
#' data(adv)
#' # from winner/loser sequence directly
#' creatematrix(winners=adv$winner, losers=adv$loser)
#' # via an eloobject
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

creatematrix <- function(eloobject, daterange=NULL, drawmethod="omit",
                         onlyinteracting=FALSE, winners, losers, draw=NULL) {
  # decide which data were supplied (elo object or winners/losers)
  if (missing(eloobject)) {
    if (!missing(winners) & !missing(losers)) funcmode <- "vec"
    if (missing(winners) | missing(losers)) funcmode <- "incomplete"
  }
  if (!missing(eloobject)) {
    if (!missing(winners) & !missing(losers)) funcmode <- "incomplete2"
    if (missing(winners) | missing(losers)) funcmode <- "elo"
  }

  # construct matrix depending on data supplied

  if (funcmode == "elo") {
    # set the date range in case it's not specified...
    if (is.null(daterange[1])) {
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
    if (sum(dataseq$draw) > 0) {

      xdata <- dataseq[dataseq$draw == TRUE, ]
      xdata <- table(xdata$winner, xdata$loser)
      if (drawmethod == "0.5") {
        xdata <- xdata / 2
        mat1[rownames(xdata), colnames(xdata)] <- xdata
        mat1 <- mat1 + t(mat1)
        mat <- mat + mat1
      }

      if (drawmethod == "1") {
        mat1[rownames(xdata), colnames(xdata)] <- xdata
        mat1 <- mat1 + t(mat1)
        mat <- mat + mat1
      }

    }

    # if "only interacting" was selected: remove those individuals from the matrix that have not interacted
    if (onlyinteracting) {
      empty <- as.numeric(which(colSums(mat) + rowSums(mat) == 0))
      if (length(empty) > 0) mat <- mat[-empty, -empty]
    }

    return(mat)
  }

  if (funcmode == "vec") {
    # all individuals in data
    allids <- sort(unique(c(as.character(winners), as.character(losers))))

    # create empty matrix
    mat <- matrix(ncol = length(allids), nrow = length(allids), 0)
    colnames(mat) <- rownames(mat) <- allids

    # tabulate outcomes
    xt <- data.frame(table(winners, losers))
    xt$winners <- as.character(xt$winners)
    xt$losers <- as.character(xt$losers)

    # fill matrix
    for (i in 1:nrow(xt)) {
      mat[xt$winners[i], xt$losers[i]] <- xt$Freq[i]
    }

    return(mat)
  }

  # or return errors if data was supplied in incomplete or false way
  if (funcmode == "incomplete") {
    stop("you need supply either an elobject OR two vectors with winners and losers")
  }
  if (funcmode == "incomplete2") {
    stop("you need supply either an elobject OR two vectors with winners and losers, not both at the same time")
  }

}
