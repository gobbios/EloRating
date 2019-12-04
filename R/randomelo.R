# randomelo 15_09_29
# used in other functions:
# ---

#' calculate Elo ratings from an interaction matrix
#'
#' calculate Elo ratings from an interaction matrix based on randomly generated sequences
#'
#' @param interactionmatrix square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#' @param runs number of randomly generated sequences based on the interactions in the \code{interactionmatrix}
#' @param normprob logical (by default \code{TRUE}). Should a normal curve be assumed for calculating the winning/losing probablities, or a logistic curve. See \code{\link{winprob}} for details
#' @param progressbar logical, should progress bars be displayed, by default \code{progressbar=TRUE}
#' @param k numeric, factor \emph{k} that determines the maximum change in ratings. By default \code{k=100}
#'
#' @return list of length 2. The first element contains a matrix with the final ratings of each individual from each random sequence. IDs are in the columns, each run is represented as one row. The second element of the list contains the original interaction matrix.
#'
#' @author Christof Neumann
#'
#' @export
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#' mat <- creatematrix(SEQ)
#' res <- randomelo(mat, 10)
#' data.frame(ID=colnames(res[[1]]), avg=round(colMeans(res[[1]]),1))

randomelo <- function(interactionmatrix, runs = 2000, normprob = TRUE,
                      k = 100, progressbar = FALSE) {
  # create a sequence from the matrix
  winner <- c()
  loser <- c()
  for (i in 1:nrow(interactionmatrix)) {
    for (j in 1:ncol(interactionmatrix)) {
      if (interactionmatrix[i, j] > 0) {
        winner <- c(winner, rep(rownames(interactionmatrix)[i], interactionmatrix[i, j]))
        loser <- c(loser, rep(colnames(interactionmatrix)[j], interactionmatrix[i, j]))
      }
    }
  }
  Date <- seq(as.Date("2000-01-01"), as.Date("2000-01-01") + length(winner) - 1, by = "day")
  # the starting sequence (which will actually not be used, but only randomized versions of it...)
  xdata <- data.frame(Date, winner, loser)
  rm(i, j, winner, loser, Date)

  res <- matrix(ncol = length(unique(c(levels(xdata$winner), levels(xdata$loser)))), nrow = runs, 0)
  colnames(res) <- unique(c(levels(xdata$winner), levels(xdata$loser)))

  if (progressbar) progbar <- txtProgressBar(min = 0, max = runs, style = 3, char = ".")

  for (i in 1:runs) {
    tempdata <- xdata[sample(1:nrow(xdata)), ]; rownames(tempdata) <- NULL
    tempres <- elo.seq(winner = tempdata$winner,
                       loser = tempdata$loser,
                       Date = tempdata$Date,
                       progressbar = FALSE,
                       runcheck = FALSE,
                       normprob = normprob,
                       k = k)
    res[i, ] <- extract_elo(tempres)[colnames(res)]
    if (progressbar) setTxtProgressBar(progbar, i)
  }

  outp <- list()
  outp[[1]] <- res
  outp[[2]] <- interactionmatrix
  class(outp) <- "randomelo"
  return(outp)
}
