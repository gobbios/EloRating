#' calculate start values from prior knowledge
#'
#' @param ranks named vector, contains the ordinal ranks of all individuals for which such prior knowledge exists, names of the vector refer to the individual codes as they occur in the interaction sequence supplied to \code{\link{elo.seq}}
#' @param rankclasses list with four items, each representing a rank class in descending order, if a given rank class is empty supply it as \code{NULL}, see details and examples
#' @param shape numeric, between 0 and 1, by default \code{shape=0.3}. This value determines the 'steepness' of the initial values. Steepest is at \code{shape=0} and shallowest is at \code{shape=1}. See examples.
#' @param startvalue numeric, the rating value with which an individual starts into the rating process. By default \code{startvalue=1000}
#' @param k numeric, the \emph{k} factor that determines the maximum change in ratings. By default \code{k=100}
#'
#' @details only one of \code{ranks} or \code{rankclasses} can be supplied.
#'
#' if you wish to supply rank classes you need to supply four categories and it is assumed that the first list item is the highest class. If you have less than four rank classes, you still need to supply a list with four items and set those that you wish to ignore to \code{NULL}, see examples.
#'
#' @return list with three items:\cr
#' \item{res}{a named numeric vector with the startvalues to be supplied to \code{\link{elo.seq}}}
#' \item{k}{\emph{k} factor used}
#' \item{startvalue}{start value used}
#' @export
#'
#' @author Christof Neumann
#'
#' @references
#' \insertRef{newton-fisher2017a}{EloRating}
#'
#' @examples
#' # assuming a group with 7 individuals
#' # with four rank classes
#' myrankclasses <- list(alpha = "a", high=c("b", "c"), mid=c("d", "e"), low=c("f", "g"))
#' createstartvalues(rankclasses = myrankclasses)
#' # with two rank classes
#' myrankclasses2 <- list(class1 = NULL, high=c("a", "b", "c"), class3=NULL, low=c("d", "e", "f", "g"))
#' createstartvalues(rankclasses = myrankclasses2)
#'
#' # with ordinal ranks
#' myranks <- 1:7; names(myranks) <- letters[1:7]
#' createstartvalues(ranks = myranks)

createstartvalues <- function(ranks = NULL, rankclasses = NULL, shape = 0.3, startvalue = 1000, k = 100) {
  # function to create startvalues from ordinal or categorical rank order (prior knowledge)
  # Ei = Se + [(xr - Sri) * k * Sri^(-Ir) ]
  # Ei = new startvalue
  # Se = start value, typically 1000
  # xr = median of supplied ranks
  # Sri = ordinal rank of individual i
  # k = k value, typically 100
  # Ir = "prior rank index" (akin to steepness)

  # if rank classes/categories are provided it is assumed that they are ordered (and names are disregarded): first list item highest category, down to lowest category

  if (!is.null(ranks) & !is.null(rankclasses)) stop("ranks and rank classes detected, please provide one or the other", call. = FALSE)

  if (!is.null(ranks)) res <- startvalue + ( (median(ranks) - ranks ) * k * ranks ^ (shape * (-1)))

  if (!is.null(rankclasses)) {
    if (length(rankclasses) == 4) {
      # total number of individuals
      N <- length(unlist(rankclasses))
      # rank definitions cf paper:
      rdefs <- c(1, N / 4, N / 2, (N - N / 4))
      # ids per class
      nperclass <- unlist(lapply(rankclasses, length))

      ranks <- unlist(sapply(1:4, function(x)rep(rdefs[x], length.out = nperclass[x])))
      names(ranks) <- unlist(rankclasses)
      res <- startvalue + ( (median(ranks) - ranks ) * k * ranks ^ (shape * (-1)))

    }
  }

  # center custion startvalues around desired startvalue
  if (mean(res) != startvalue) res <- res - mean(res) + startvalue

  # and round ratings
  res <- round(res)

  # add k and startvalue to the output, to make sure that the elo.seq function
  # uses the same values [which in the current implementation is not actually used]
  res <- list(res = res, k = k, startvalue = startvalue)

  return(res)
}
