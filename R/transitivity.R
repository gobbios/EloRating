#' triangle transitivity
#'
#' @param m square dominance matrix
#' @param runs numeric, the number of runs for the randomization test
#' @param returnfig logical, should a figure be produced that shows the distribution of expectation
#'
#' @importFrom sna rguman triad.census dyad.census
#' @importFrom network network
#' @importFrom graphics box hist
#'
#' @references
#' \insertRef{shizuka2012}{EloRating}
#'
#' \href{http://www.shizukalab.com/toolkits/sna/triangle-transitivity}{http://www.shizukalab.com/toolkits/sna/triangle-transitivity}
#'
#' @return a named vector of length four
#' @export
#'
#' @examples
#' data(bonobos)
#' transitivity(bonobos)

transitivity <- function(m, runs = 2000, returnfig = FALSE) {
  # function based on supplement from Shizuka and McDonald 2012 Anim Behav
  # requires statnet package
  # one modification: for randomizations, also the weighting vector is used

  int.to.dom <- function(x) {
    ( (x > t(x) ) & ( x + t(x) > 0) ) + 0
  }

  res <- rep(NA, 4)
  names(res) <- c("Pt", "ttri", "p", "runs")
  res[4] <- runs

  weightfac <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0.5, 0.75, 0.75)
  sumfac <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1)

  tri <- triad.census(network(int.to.dom(m), directed = TRUE))

  res[1] <- sum(tri * weightfac) / sum(tri * sumfac)
  res[2] <- 4 * (res[1] - 0.75)

  # if(!sigtest) {
  #   # tri <- triad.census(network(int.to.dom(m), directed = TRUE))
  #   # #tri <- sna::triad.census(network(int.to.dom(m),directed=TRUE))
  #   #
  #   # #w <- as.vector() # The weighting vector for transitivity
  #   # #N.triangle <-  #Count and sum the number of triangles
  #   # Pt <- sum(tri * weightfac) / sum(tri * sumfac)
  #   # if(!returnprop) Pt <- 4 * (Pt - 0.75)
  #   # return(Pt)
  #
  # }

  # randomization procedure
  n <- nrow(m)
  m <- network(int.to.dom(m), directed = TRUE)
  dyads <- dyad.census(m)
  triads <- triad.census(m)
  Pt <- sum(triads * weightfac) / sum(triads * sumfac)
  rPt <- numeric(runs)

  j <- 0
  while (j < runs){
    r <- rguman(1, nv = n, mut = dyads[1], asym = dyads[2], null = dyads[3])
    r.triad <- triad.census(r)
    rPt[j + 1] <- sum(r.triad * weightfac) / sum(r.triad * sumfac)
    if (is.na(rPt[j + 1])) next else j <- j + 1
  }
  res[3] <- length(rPt[rPt >= Pt]) / runs

  if (returnfig) {
    x <- hist(rPt, plot = FALSE)
    hist(rPt, main = "expected triangle transitivity", xlab = "triangle transitivity",
         yaxs = "i", las = 1, xlim = c(-0.01, 1.01), xaxs = "i",
         ylim = c(0, max(x$counts) * 1.05))
    box()
    axis(1, at = Pt, col = "red", labels = NA, lwd.ticks = 5, tcl = 2)
  }


  # if(sigtest) {
  #   n <- nrow(m)
  #   m <- network(int.to.dom(m), directed = TRUE)
  #   dyads <- dyad.census(m)
  #   triads <- triad.census(m)
  #   # dyads <- sna::dyad.census(m)
  #   # triads <- sna::triad.census(m)
  #   #
  #   Pt <- sum(triads * weightfac) / sum(triads * sumfac)
  #
  #   #Pt <- triads[9]/(triads[10]+triads[9])
  #
  #   rPt <- numeric(runs)
  #
  #   j=0
  #   while(j<runs){
  #     r <- rguman(1, nv = n, mut = dyads[1], asym = dyads[2], null = dyads[3])
  #     r.triad <- triad.census(r)
  #     # r.triad <- sna::triad.census(r)
  #     rPt[j + 1] <- sum(r.triad * weightfac) / sum(r.triad * sumfac)
  #     # r.triad[9]/(r.triad[10]+r.triad[9])
  #     if (is.na(rPt[j + 1])) next else j <- j + 1
  #   }
  #   p <- length(rPt[rPt >= Pt]) / runs
  #
  #   if(returnfig) {
  #     x <- hist(rPt, plot = FALSE)
  #     hist(rPt, main = "expected triangle transitivity", xlab = "triangle transitivity", yaxs = "i", las = 1, xlim = c(-0.01, 1.01), xaxs = "i", ylim = c(0, max(x$counts) * 1.05))
  #     box()
  #     axis(1, at = Pt, col = "red", labels = NA, lwd.ticks = 5, tcl = 2)
  #   }
  #   res <- c(Pt, p)
  #   names(res) <- c("Pt", "p")
  #   return(res)
  # }

  return(round(res, 3))
}
