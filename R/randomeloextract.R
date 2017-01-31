# library(EloRating)
# data(adv)
# SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
# mat <- creatematrix(SEQ)
# x <- randomelo(mat, 20)
# ID="a"

#' extract ratings from random sequences based on an interaction matrix
#'
#' extract ratings from random sequences based on an interaction matrix
#'
#' @param x output from \code{\link{randomelo}}
#' @param ID character, ID
#' @param mode character, one of three: 1) \code{"obj"}: a random value from all the sequences; 2) \code{"samp"}: a random value sampled from a normal distribution with mean and sd of all randomized values; 3) \code{"avg"}: the average value from all the runs
#'
#' @return numeric
#'
#' @author Christof Neumann
#'
#' @export
#'
#' @importFrom stats rnorm
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#' mat <- creatematrix(SEQ)
#' res <- randomelo(mat, 10)
#' randomeloextract(res, "a", "samp")
#' randomeloextract(res, "a", "obj")
#' randomeloextract(res, "a", "avg")

randomeloextract <- function(x, ID, mode=c("obj", "samp", "avg")) {
  if(!ID %in% colnames(x[[1]])) stop("selected ID is not in the matrix", call.=FALSE)

  if(mode=="obj") {
    return(sample(x[[1]][, ID], 1))
  }

  if(mode=="samp") {
    rd <- rnorm(1000, mean(x[[1]][,ID]), sd(x[[1]][,ID]))
    return(round(sample(rd,1), 1))
  }

  if(mode=="avg") {
    return(round(mean(x[[1]][, ID]), 1))
  }

}


# temp <- data.frame(ID=NA, samp=rep(0, 50), obj=rep(0,50))
# for(i in 1:nrow(temp)) {
#   ID <- sample(colnames(x[[1]]), 1)
#   temp$samp[i] <- randomeloextract(x, ID, "samp")
#   temp$obj[i] <- randomeloextract(x, ID, "obj")
# }
# plot(temp$obj, temp$samp)
# cor.test(temp$obj, temp$samp)