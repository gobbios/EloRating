# e.single 15_09_29
# used in other functions:
# elo.seq()
# randomelo()


#' Elo ratings for a single interaction
#'
#' calculate/update Elo ratings for a single dyadic interaction
#'
#' @param ELO1old,ELO2old Elo rating of the first and second individual
#' @param outcome "1" = first individual wins and second looses\cr
#' "2" = second individual wins and first looses\cr
#' "0" = interaction ends in a draw/tie (no winner and no looser)
#' @param k \emph{k} factor, by default \code{k = 100}
#' @param normprob logical (by default \code{TRUE}). Should a normal curve be assumed for calculating the winning/losing probablities, or a logistic curve. See \code{\link{winprob}} for details
#'
#' @return integer vector of length 2 with updated ratings of first and second individual after the interaction
#'
#' @references Albers, P. C. H. & de Vries, H. 2001. Elo-rating as a tool in the sequential estimation of dominance strengths. Animal Behaviour, 61, 489-495. (\href{https://dx.doi.org/10.1006/anbe.2000.1571}{DOI: 10.1006/anbe.2000.1571})
#'
#' @importFrom stats pnorm
#'
#' @author Christof Neumann
#'
#' @examples
#' e.single(ELO1old=1200, ELO2old=1000, outcome=1, k=100)
#' # same as before
#' e.single(ELO1old=1000, ELO2old=1200, outcome=2, k=100)
#' # an undecided interaction
#' e.single(ELO1old=1200, ELO2old=1000, outcome=0, k=100)
#' # if rating differences are too big, no change occurs
#' # if higher-rated individual wins
#' e.single(ELO1old=2000, ELO2old=1000, outcome=1, k=100)
#' # same as before but lower-rated individual wins and
#' # therefore wins maximum number of points possible (i.e. k)
#' e.single(ELO1old=2000, ELO2old=1000, outcome=2, k=100)
#'
#' @export



e.single <- function(ELO1old, ELO2old, outcome, k=100, normprob=TRUE) {

  # outcome must be one of the following:
  # "1" = first individual wins and second looses
  # "2" = second individual wins and first looses
  # "0" = interaction ends in a draw/tie (no winner and no looser)

  # normprob:
  # should winning probability calculated from normal curve or logistic curve?

  # make sure ELO ratings are given as numerics
  # omit because inside functions this is true...
  # ELO1old <- as.numeric(as.character(ELO1old))
  # ELO2old <- as.numeric(as.character(ELO2old))

  # calculates the difference between the two ratings
  ELO_diff <- (ELO1old - ELO2old)

  if(normprob) {
    # z score based on fixed SD=200 (see Elo 1978)
    z_score <- ELO_diff/(200*sqrt(2))
    # calculates the winning probabilty
    p_win <- pnorm(z_score)
  } else {
    # winning probability based on logistic approach
    p_win <- 1 - 1 / (1 + 10^(ELO_diff/400))
  }


  # product of winning probabilty and k-factor
  kp <- k * p_win

  # the actual updating calculations
  if(outcome==1) { ELO1new <- ELO1old - kp + k
  ELO2new <- ELO2old + kp - k
  }
  if(outcome==2) { ELO1new <- ELO1old - kp
  ELO2new <- ELO2old + kp
  }
  if(outcome==0) { ELO1new <- ELO1old - kp + 0.5 * k
  ELO2new <- ELO2old + kp - 0.5 * k
  }
  # returning the updated ratings
  # ratings are rounded to integers
  return(round(c(ELO1new, ELO2new), 0))
}
