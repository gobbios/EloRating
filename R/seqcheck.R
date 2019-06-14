# seqcheck 14_11_19

#' runs raw data diagnostics for Elo rating
#'
#' runs some diagnostics on the data supplied to \link{elo.seq}, to check whether \link{elo.seq} will run without errors
#'
#' @param winner either a factor or character vector with winner IDs of dyadic dominance interactions
#' @param loser either a factor or character vector with loser IDs of dyadic dominance interactions
#' @param Date character vector of form "YYYY-MM-DD" with the date of the respective interaction
#' @param draw logical, which interactions ended undecided (i.e. drawn or tied)? By default all \code{FALSE}, i.e. no undecided interactions occurred. Note that in this case, \code{winner}/\code{loser} values can be interchanged
#' @param presence optional data.frame, to supply data about presence and absence of individuals for part of the time the data collection covered. see details
#'
#' @details calender dates (for the sequence as well as in the first column of \code{presence}, if supplied) need to be in "YYYY-MM-DD" format!
#'
#' \code{seqcheck} will return two types of messages: warnings and errors. Errors will result in the data NOT working when supplied to \code{elo.seq}, and need to be fixed. Warning message do not necessarily lead to failure of executing \code{elo.seq}. Note that by default \code{seqcheck} is part of \code{elo.seq}. If any error or warning is produced by \code{seqcheck}, these data will not work in \code{\link{elo.seq}}. Some warning (but not error) messages can be ignored (see below) and if the \code{runcheck} argument in \code{elo.seq} is set to \code{FALSE} Elo-ratings will be calculated properly in such cases.
#'
#' The actual checks (and corresponding messages) that are performed are described in more detail here:
#'
#' Most likely (i.e. in our experience), problems are caused by mismatches between the interaction data and the corresponding presence data.
#'
#' Errors:\cr
#' \code{Presence starts AFTER data}: indicates that during interactions at the beginning of the sequence, no corresponding information was found in the presence data. Solution: augment presence data, or remove interactions until the date on which presence data starts
#'
#' \code{Presence stops BEFORE data}: refers to the corresponding problem towards the end of interaction and presence data
#'
#' \code{During the following interactions, IDs were absent...}: indicates that according to the presence data, IDs were absent (i.e. "0"), but interactions with them occured on the very date(s) according to the interaction data
#'
#' \code{The following IDs occur in the data sequence but NOT...}: there is/are no columns corresponding to the listed IDs in the presence data
#'
#' \code{There appear to be gaps in your presence (days missing?)...}: check whether your presence data includes a line for \emph{each date} starting from the date of the first interaction through to the date of the last interaction
#'
#' Warnings:
#'
#' \code{Presence continues beyond data}: indicates that presence and interaction data do not end on the same date.
#'
#' \code{Presence starts earlier than data}: indicates that presence and interaction data do not start on the same date.
#'
#' \code{The following IDs occur in the presence data but NOT...}: there are more ID columns in the presence data than IDs occuring in the interaction data
#'
#' \code{Date column is not ordered}: The dates are not supplied in ascending order. \code{\link{elo.seq}} will still work but the results won't be reliable because the interactions were not in the correct sequence.
#'
#' Other warnings/errors can result from inconsistencies in either the presence or sequence data, or be of a more general nature:
#'
#' Errors:
#'
#' \code{No 'Date' column found}: in the presence data, no column exists with the name/header "Date". Please rename (or add) the necessary column named "Date" to your presence data.
#'
#' \code{At least one presence entry is not 1 or 0}: presence data must come in binary form, i.e. an ID was either present ("1") or absent ("0") on a given date. No \code{NA}s or other values are allowed.
#'
#' \code{Your data vectors do not match in length}: at least one of the three mandatory arguments (winner, loser, Date) differs from one other in length. Consider handling your data in a data.frame, which avoids this error.
#'
#' Warnings:
#'
#' \code{IDs occur in the data with inconsistent capitalization}: because \code{R} is case-sensitive, "A" and "a" are considered different individuals. If such labelling of IDs is on purpose, ignore the warning and set \code{runcheck=FALSE} when calling \code{elo.seq()}
#'
#' \code{There is (are) X case(s) in which loser ID equals winner ID}: winner and loser represent the same ID
#'
#' \code{The following individuals were observed only on one day}: while not per se a problem for the calculation of Elo ratings, individuals that were observed only on one day (irrespective of the number of interactions on that day) cannot be plotted. \code{\link{eloplot}} will give a warning in such cases, too.
#'
#' @return returns textual information about possible issues with the supplied data set, or states that data are fine for running with \code{\link{elo.seq}}
#'
#' @author Christof Neumann
#' @export
#'
#' @examples
#' data(adv)
#' seqcheck(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' data(advpres)
#' seqcheck(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'          presence = advpres)
#'
#' # create faulty presence data
#' # remove one line from presence data
#' faultypres <- advpres[-1, ]
#' # make all individuals absent on one day
#' faultypres[5, 2:8] <- 0
#' # run check
#' seqcheck(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'          presence = faultypres)
#'
#' # fix first error
#' faultypres <- rbind(faultypres[1, ], faultypres)
#' faultypres$Date[1] <- "2010-01-01"
#'
#' # run check again
#' seqcheck(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'          presence = faultypres)
#'
#' # fix presence on date for interaction number 6
#' faultypres[6, 2:8] <- 1
#'
#' # run check again
#' seqcheck(winner = adv$winner, loser = adv$loser, Date = adv$Date,
#'          presence = faultypres)
#' # all good now


seqcheck <- function(winner, loser, Date, draw = NULL, presence = NULL) {

  # Part 1:
  # check some general issues

  # creating checksum
  checksum <- rep(0, 15)
  names(checksum) <- c("IDcheck", "selfinteractions", "presence", "startpresence1", "startpresence2", "endpresence1", "endpresence2", "IDmatch", "IA_presencematch", "presenceentries", "datecol", "length", "singledayobs", "continouspres", "seqdateorder")

  Date <- as.Date(as.character(Date))
  # check whether dates for the interactions are in increasing order
  checkval <- min(as.numeric(diff(Date, unit = "day")))
  if (checkval < 0) checksum["seqdateorder"] <- 1

  # check whether all vectors contain same number of entries
  if (length(winner) != length(loser) | length(winner) != length(Date)) checksum["length"] <- 1


  # check draw/tie vector
  if (is.null(draw)) draw <- rep(FALSE, length(winner))
  if (length(winner) != length(draw)) checksum["length"] <- 1


  # the remaining checks are conditional on the fact that length of vectors match...

  if (checksum["length"] == 0) {

    datasequence <- data.frame(Winner = winner, Loser = loser, Date = Date)

    # check for integrity of IDs
    winners <- as.character(datasequence[, "Winner"])
    losers <- as.character(datasequence[, "Loser"])
    allIDs <- sort(unique(c(winners, losers)))
    if (length(allIDs) == length(unique(tolower(allIDs)))) {
      IDcheck <- "Everything seems alright with capitalization of IDs"
    } else {
      IDcheck <- "There seems to be a problem with capitalization of IDs?"
      checksum["IDcheck"] <- 1
    }

    # check whether IDs had interactions with themselves...
    selfinteractions <- paste("There is (are)",
                              length(which(winners == losers)),
                              "case(s) in which loser ID equals winner ID")
    if (length(which(winners == losers)) > 0) checksum["selfinteractions"] <- 1

    # check whether presence data is given
    if (!is.null(presence)) {
      presenceD <- "Presence data is supplied"
    } else {
      presenceD <- "Presence data is not supplied"
      checksum["presence"] <- 1
    }

    # check whether there is a column named Date in the presence matrix
    if (!is.null(presence)) {
      if ("Date" %in% colnames(presence)) {
        datecol <- "Date column found"
      } else {
        datecol <- "No 'Date' column found in supplied presence data"
        checksum["datecol"] <- 1
      }
    }

    # check whether there are gaps in the presence data...
    if (!is.null(presence) & checksum["datecol"] == 0) {
      if (nrow(presence) < as.numeric(diff(range(presence$Date))) + 1) {
        checksum["continouspres"] <- 1
        continouspres <- "There appear to be gaps in your presence data (missing days?)"
      }
    } else {
      continouspres <- "not checked"
    }



    # check whether date range in presence is the same as in sequence data
    START <- NA
    END <- NA
    if (!is.null(presence) & checksum["datecol"] == 0) {
      DATESpres <- as.Date(as.character(presence[, which(colnames(presence) %in% c("Date", "date"))]))
      DATESdata <- unique(as.Date(as.character(datasequence[, which(colnames(datasequence) %in% c("Date", "date"))])))
      if (min(DATESpres) < min(DATESdata)) {
        START <- "Presence starts earlier than data"
        checksum["startpresence1"] <- 1
      } # actually, not a problem per se
      if (min(DATESpres) > min(DATESdata)) {
        START <- "Presence starts AFTER data -> PROBLEM!"
        checksum["startpresence2"] <- 1
      }
      if (min(DATESpres) == min(DATESdata)) {
        START <- "Presence starts at the same date than data -> GOOD!"
      }
      if (max(DATESpres) < max(DATESdata)) {
        END <- "Presence stops BEFORE data -> PROBLEM!"
        checksum["endpresence1"] <- 1
      }
      if (max(DATESpres) > max(DATESdata)) {
        END <- "Presence continues beyond data"
        checksum["endpresence2"] <- 1
      } # actually, not a problem per se
      if (max(DATESpres) == max(DATESdata)) {
        END <- "Presence stops at the same date than data -> GOOD!"
      }
    }

    # check whether IDs match in data and presence
    if (!is.null(presence)) {
      IDdata <- sort(allIDs)
      IDpres <- sort(names(presence[, 2:ncol(presence)]))

      IDmatch <- "IDs in presence and data match -> GOOD!"

      # check whether
      if (length(which(!IDpres %in% IDdata)) > 0) {
        IDmatch1 <- IDpres[which(!IDpres %in% IDdata)]
      } else {
        IDmatch1 <- "none"
      }

      if (length(which(!IDdata %in% IDpres)) > 0) {
        IDmatch2 <- IDdata[which(!IDdata %in% IDpres)]
      } else {
        IDmatch2 <- "none"
      }

      if (IDmatch1[1] != "none" | IDmatch2[1] != "none") {
        IDmatch <- c(paste("The following IDs occur in the presence data but NOT in the data sequence:", IDmatch1), paste("the following IDs occur in the data sequence but NOT in the presence data:", IDmatch2))
        checksum[8] <- 1
      }

    }

    # check whether IDs were actually present on the dates of their interactions
    # note that IDs that occur in the data sequence BUT NOT in the presence are ignored here!
    IA_presencematch  <- NA
    IA_presencematchN <- NA

    nmatrows <- length(seq(from = min(as.Date(Date)), to = max(as.Date(Date)), by = "day"))
    if (!is.null(presence) & checksum["datecol"] == 1) checksum["IA_presencematch"] <- 2

    if (!is.null(presence) & checksum["datecol"] == 0) {
      if (nmatrows == nrow(presence)) {
        IA_presencematch <- c()
        for (i in 1:nrow(datasequence)) {
          if (sum(!(IDmatch2 %in% c(winners[i], losers[i]))) > 0 & IDmatch2[1] == "none") {
            if (sum(presence[which(datasequence[i, "Date"] == presence[, "Date"]), c(winners[i], losers[i])], na.rm = TRUE) != 2) {
              IA_presencematch <- c(IA_presencematch, i)
            }
          }
        }
        if (is.null(IA_presencematch)) {
          IA_presencematch <- "All IDs were present on their interaction dates"
          IA_presencematchN <- 0
        } else {
          IA_presencematchN <- IA_presencematch
          IA_presencematch <- paste("During", length(IA_presencematchN), "interactions, IDs were absent according to the presence data:", sep = " ")
          checksum["IA_presencematch"] <- 1
        }
      }
    }

    # check whether there are entries other than 0's and 1's in the presence
    if (!is.null(presence)) {
      temp <- as.numeric(apply(presence[, 2:ncol(presence)], 2, function(xx) length(which(xx == 0 | xx == 1))))
      presenceentries <- "All presence entries are either 1 or 0 --> GOOD"
      if (length(which(temp != nrow(presence))) > 0)  {
        presenceentries <- "At least one presence entry is not 1 or 0 --> PROBLEM"
        checksum["presenceentries"] <- 1
      }
    }

    # check for cases in which IDs were observed only on a single day (even though multiple times is possible, but that doesnt make a difference...)
    temp <- rbind(rowSums(table(winner, Date) > 0)[allIDs], rowSums(table(loser, Date) > 0)[allIDs])
    colnames(temp) <- allIDs
    sIDs <- "none"
    if (1 %in% colSums(temp, na.rm = TRUE)) {
      checksum["singledayobs"] <- 1
      sIDs <- colnames(temp)[colSums(temp, na.rm = TRUE) == 1]
    }


    if (!is.null(presence)) {
      res <- list(checksum = checksum, IDcheck = IDcheck, selfinteractions = selfinteractions, presence = presenceD, startpresence = START, endpresence = END, IDmatch = IDmatch, IA_presencematch = IA_presencematch, IA_presencematchN = IA_presencematchN, presenceentries = presenceentries, IDmatch1 = IDmatch1, IDmatch2 = IDmatch2, datecol = datecol, singledaycases = sIDs)
      class(res) <- "sequencecheck"
    }
    if (is.null(presence)) {
      res <- list(checksum = checksum, IDcheck = IDcheck, selfinteractions = selfinteractions, presence = presenceD, singledaycases = sIDs, continouspres = continouspres)
      class(res) <- "seqchecknopres"
    }


  } # end part (conditional on vector length match)

  if (checksum["length"] == 1) {
    res <- list(checksum = checksum, IDcheck = NA, selfinteractions = NA, presence = NA)
    class(res) <- "seqchecknopres"
  }

  return(res)
}
