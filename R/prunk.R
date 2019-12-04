#' unknown relationships
#'
#' unknown relationships
#'
#' @param eloobject output from \code{\link{elo.seq}} or a matrix, e.g. from \code{\link{creatematrix}}
#' @param daterange date range to be considered (character or Date of length 2), by default considers the entire date range of the sequence. In case the function works on a matrix this is ignored.
#'
#' @return numeric, proportion of unknown relationships (and total N) when considering all possible dyads, and the same after accounting for co-residency. For matrices, considering co-residency is ignored.
#'
#' @author Christof Neumann
#'
#' @export
#'
#' @examples
#' data(adv); data(advpres)
#' x <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date, presence = advpres)
#' prunk(x, c("2010-01-01", "2010-01-15"))
#' mat <- creatematrix(x, c("2010-01-01", "2010-01-15"))
#' prunk(mat)

prunk <- function(eloobject, daterange = NULL) {

  if (inherits(x = eloobject, what = "elo")) {
    # get all ids, the sequence, and the presence matrix
    xd <- eloobject$logtable
    xd$Date <- as.Date(eloobject$truedates[xd[, 1]])
    pres <- data.frame(Date = eloobject$truedates, eloobject$pmat)
    pres2 <- eloobject$pmat

    # and set the date range in case it's not specified...
    if (is.null(daterange[1])) {
      daterange <- c(min(xd$Date), max(xd$Date))
    } else {
      daterange <- as.Date(daterange)
    }

    # and limit the data sets accordingly
    xd <- xd[xd$Date <= daterange[2] & xd$Date >= daterange[1], ]
    pres2 <- pres2[pres$Date <= daterange[2] & pres$Date >= daterange[1], ]
    pres <- pres[pres$Date <= daterange[2] & pres$Date >= daterange[1], ]

    # and remove columns of IDs that were not yet present
    if (0 %in% colSums(pres2)) {
      pres2 <- pres2[, -c(which(colSums(pres2) == 0))]
      ids <- colnames(pres2)
      pres <- pres[, c("Date", ids)]
    }

    # recreate eloobject and some info about how the supplied ratings were calculated
    startval <- as.numeric(eloobject$misc["startvalue"])
    kval <- as.numeric(eloobject$misc["k"])
    init <- as.character(eloobject$misc["init"])
    eloobject <- elo.seq(winner = xd$winner,
                         loser = xd$loser,
                         Date = xd$Date,
                         draw = xd$draw,
                         presence = pres,
                         startvalue = startval,
                         k = kval,
                         init = init,
                         progressbar = FALSE,
                         runcheck = FALSE)

    # create the matrix, and make sure also IDs that didn't interact but were present are included
    mat <- creatematrix(eloobject, drawmethod = "omit")
    if (length(intersect(colnames(pres2), colnames(mat))) < length(colnames(pres2))) {
      add <- colnames(pres2)[which(!colnames(pres2) %in% colnames(mat))]
      for (i in 1:length(add)) {
        mat <- cbind(mat, 0); colnames(mat)[ncol(mat)] <- add[i]
        mat <- rbind(mat, 0); rownames(mat)[nrow(mat)] <- add[i]
      }
    }

    mat2 <- mat
    # create table of dyads and check which of the possible dyads were never coresident...
    co <- cbind(t(combn(colnames(mat), 2)), NA)
    for (i in 1:nrow(co)) {
      if (max(rowSums(pres[, co[i, 1:2]])) < 2) {
        mat2[co[i, 1], co[i, 2]] <- mat2[co[i, 2], co[i, 1]] <- NA
      }
    }

    # get dyadic values of interactions
    # first without taking coresidenec into account
    res <- mat[upper.tri(mat, diag = FALSE)] + t(mat)[upper.tri(t(mat), diag = FALSE)]
    N <- length(res)
    pu <- length(which(res == 0)) / N

    # and with those dyads that were not coresident set to NA (those are excluded)
    res <- na.omit(mat2[upper.tri(mat2, diag = FALSE)] + t(mat2)[upper.tri(t(mat2), diag = FALSE)])
    N2 <- length(res)
    pup <- length(which(res == 0)) / N2

    res <- c(round(pu, 3), N, round(pup, 3), N2)
    names(res) <- c("pu.all", "dyads.all", "pu.cores", "dyads.cores")

  }

  if (inherits(x = eloobject, what = "matrix")) {
    up <- eloobject[upper.tri(eloobject)]
    lo <- t(eloobject)[upper.tri(eloobject)]
    res <- c(round(sum(up + lo == 0) / length(lo), 3), length(lo), NA, NA)
    names(res) <- c("pu.all", "dyads.all", "pu.cores", "dyads.cores")
  }

  return(res)
}
