#' individuals present in the group
#'
#' returns IDs, number or IDs, or CV of number of present individuals
#'
#' @param eloobject result from \code{\link{elo.seq}}
#' @param from character, from which date onwards should the ID statistics be calculated. By default the first date in the sequence is used
#' @param to character, until which date should the ID statistics be calculated. By default \code{NULL}, i.e. the returned information refers to only the date specified by \code{from}
#' @param outp character, one of three options to determine which kind of information is returned: (1) \code{"N"}: the (average) number of individuals present, (2) \code{"IDs"}: the actual IDs, and (3): \code{"CV"}: coefficient of number of individuals present
#'
#' @importFrom stats sd
#' @return numeric or character
#'
#' @author Christof Neumann
#'
#' @details if \code{to=NULL}, either the IDs (\code{outp="IDs"}) or the number of individuals (\code{outp="N"}) present on this day is returned. \code{outp="CV"} is not defined in such a case (returns \code{NA}).
#'
#' if a \code{to} date is set (i.e. different from \code{NULL}), either the IDs of all individuals that were present on at least one day of the date range (\code{outp="IDs"}) is returned or the average number of individuals present during this time (\code{outp="N"}). If \code{outp="CV"}, the coefficient of variation of the number of individuals present is returned, which might be considererd another measure of stability on the group level.
#'
#' @export
#'
#' @examples
#' data(adv)
#' SEQ <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' individuals(SEQ, outp = "N")
#' individuals(SEQ, outp = "IDs")
#' individuals(SEQ, outp = "CV") # not defined
#'
#' # consider additional presence information
#' data(advpres)
#' SEQ <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'                presence = advpres)
#' individuals(SEQ, outp = "N")
#' individuals(SEQ, outp = "IDs")
#' individuals(SEQ, outp = "CV") # not defined
#'
#' # across a date range
#' individuals(SEQ, from = "2010-01-01", to = "2010-01-31", outp = "N")
#' individuals(SEQ, from = "2010-01-01", to = "2010-01-31", outp = "IDs")
#' individuals(SEQ, from = "2010-01-01", to = "2010-01-31", outp = "CV")

individuals <- function(eloobject, from = eloobject$misc["maxDate"], to = NULL,
                        outp = c("N", "IDs", "CV")) {
  # outp:
  # N - (mean) number of individuals
  # IDs - IDs that were present on at least one of the day(s)
  # CV - coefficient of variation of N

  # some checks and prelims
  outp <- match.arg(outp)

  if (!is.null(to)) {
    if (to == from) to <- NULL
    #  if(as.Date(to)<as.Date(from)) stop("the 'to' date lies before the starting ('from') date")
  }

  # create vector with all dates (according to date range in eloobject)
  DR <- seq(from = as.Date(eloobject$misc["minDate"]),
            to = as.Date(eloobject$misc["maxDate"]),
            by = "day")

  # presence matrix
  pmat <- eloobject$pmat

  # if no 'to' date is given,
  # i.e. only one day (also the case if 'to' is the same as 'from')
  if (is.null(to)) {
    l <- which(DR == as.Date(from))
    if (outp == "N")   res <- sum(pmat[l, ])
    if (outp == "CV")  res <- NA
    if (outp == "IDs") res <- names(pmat[l, which(pmat[l, ] == 1)])
  }

  # date range is given, i.e. 'to' is other than NULL (and other than 'from')
  if (!is.null(to)) {
    l <- which(DR %in% seq(from = as.Date(from), to = as.Date(to), by = "day"))
    if (outp == "N")   res <- mean(rowSums(pmat[l, ]))
    if (outp == "CV")  res <- sd(rowSums(pmat[l, ])) / mean(rowSums(pmat[l, ]))
    if (outp == "IDs") res <- names(which(colSums(pmat[l, ]) >= 1))
  }

  return(res)
}
