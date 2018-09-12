#' calculate Elo ratings
#'
#' calculate Elo ratings from a sequence of dominance interactions
#' @aliases fastelo
#' @usage elo.seq(winner, loser, Date, draw = NULL, presence = NULL, startvalue = 1000,
#'                k = 100, normprob = TRUE, init = "average", intensity = NULL,
#'                iterate = 0, runcheck = TRUE, progressbar = FALSE)
#' fastelo(WINNER, LOSER, ALLIDS, KVALS, STARTVALUES, NORMPROB = TRUE, ROUND = TRUE)
#' @param winner either a factor or character vector with winner IDs of dyadic dominance interactions
#' @param loser either a factor or character vector with loser IDs of dyadic dominance interactions
#' @param Date character vector of form "YYYY-MM-DD" with the date of the respective interaction
#' @param draw logical, which interactions ended undecided (i.e. drawn or tied)? By default all \code{FALSE}, i.e. no undecided interactions occurred. Note that in this case, \code{winner}/\code{loser} values can be interchanged
#' @param presence optional data.frame, to supply data about presence and absence of individuals for part of the time the data collection covered. see details
#' @param startvalue the value of Elo ratings of the two individuals that are involved in the first interaction of the overall sequence prior to this interaction. By default set to 1000. See also \code{init}
#' @param k factor \emph{k} that determines the maximum change in ratings. By default \code{k=100}
#' @param normprob logical (by default \code{TRUE}). Should a normal curve be assumed for calculating the winning/losing probablities, or a logistic curve. See \code{\link{winprob}} for details
#' @param init character, what Elo rating does an individual have prior to its first interaction. Three options are available:
#' \code{average}: individuals always start with the value specified in \code{startvalue}. Given stable composition of the group, this also reflects the average Elo rating on each day in that group, \cr
#' \code{bottom}: subjects entering at the current lowest Elo value if the lowest value getting lower its getting lower for all subjects which had this lowest values before, it is reflecting that in some species new subjects entering a group at the bottom level "bottom entry"\cr
#' \code{bottom_low}: same as \code{bottom} but additionally the start values getting after the first interaction lower for all non-interacting subjects and, reflecting that we have at start no knowledge about the subjects this option offers for "bottom entry" species the possibility to consider that in a way that those subjects which are not interacting getting lower from start on
#' @param intensity a character vector or factor describing intensity of interaction, to be matched with custom k values if specified
#' @param iterate not yet implemented
#' @param runcheck logical, should several checks regarding data integrety be performed, by default \code{TRUE}. See \code{\link{seqcheck}}
#' @param progressbar logical, should progress bars be displayed, by default \code{progressbar=TRUE}
#' @param WINNER same as \code{winner} for use in \code{fastelo()}
#' @param LOSER same as \code{loser} for use in \code{fastelo()}
#' @param ALLIDS character vector, contains all the indivuals IDS
#' @param KVALS numeric vector of the same length \code{WINNER}, i.e. one k value for each interaction
#' @param STARTVALUES numeric vector of the same length as \code{ALLIDS}, i.e. one start value for each individual
#' @param NORMPROB logical, by default \code{TRUE}: same as \code{normprob} for use in \code{fastelo()}
#' @param ROUND logical, by default \code{TRUE}: should ratings be rounded to integers. For use in \code{fastelo()}
#' @details The presence 'matrix' is actually an object of class \code{data.frame} containing information about wether an individual was present on a given day or not. The first column represents the dates, running at least from the date of the earliest interaction until at least the date of the last interaction with one line per day (regardless of whether there were actually interactions observed on each day). Further, each individual is represented as a column in which "1" indicates an individual was present on the row-date and a "0" indicates the individuals absence on this date. \code{NA}s are not allowed. See \code{\link{advpres}} for an example.
#'
#' The function \code{fastelo()} is a stripped-down version of \code{elo.seq()}, which performs only the most basic calculations while ignoring anything that is date and presence related. Neither does it perform data checks. In other words, it just calculates ratings based on the sequence. It's most useful in simulations, for example when estimating optimal k parameters. Its main advantage is its speed, which is substantially faster than \code{elo.seq()}. Note that currently there is no support for tied interactions. The main difference to note is that both, start values and k values have to be supplied as vectors with one value for each individual and interaction respectively.
#'
#' @return An object of class \code{elo}, which is list with 10 items that serves as basis to extract relevant information:
#' \item{mat}{a date by ID-\code{matrix} with raw Elo ratings}
#' \item{lmat}{a date by ID-\code{matrix} with raw Elo ratings}
#' \item{cmat}{a date by ID-\code{matrix} with raw Elo ratings}
#' \item{pmat}{a date by ID-\code{matrix} with with presence data}
#' \item{nmat}{a date by ID-\code{matrix} containing the number of interactions a given ID was involved in on a given day}
#' \item{logtable}{details on each single interaction}
#' \item{stability}{a \code{data.frame} containing information about stability (see \code{\link{stab_elo}}) }
#' \item{truedates}{vector of class \code{Date} covering the ranges of dates in the dataset}
#' \item{misc}{various}
#' \item{allids}{a (sorted) character vector with all IDs that occur in the dataset}
#'
#' \code{fastelo()} returns a list with ten items:
#' \item{\code{$ratings}}{numeric vector of the final ratings in the same order as \code{ALLIDS}}
#' \item{\code{$winprobs}}{numeric vector with winning probabilities in the same order as the interactions were supplied}
#' \item{\code{$rtype}}{character of length 1, as a marker that the result comes from \code{fastelo()}}
#' \item{\code{$startvalues}}{numeric vector with start values}
#' \item{\code{$kvalues}}{numeric vector with k values}
#' \item{\code{$winner}}{character vector with winners}
#' \item{\code{$loser}}{character vector with losers}
#' \item{\code{$allids}}{character vector with all IDs that occur in the sequence}
#' \item{\code{$normprob}}{logical, was normal probability used for winning expectations}
#' \item{\code{$round}}{logical, was rounding to integers used during the calculation of ratings}
#'
#' @references
#' \insertRef{elo1978}{EloRating}
#'
#' \insertRef{albers2001}{EloRating}
#'
#' \insertRef{neumann2011}{EloRating}
#'
#' \insertRef{newton-fisher2017a}{EloRating}
#'
#' @author Christof Neumann and Lars Kulik
#'
#' @importFrom stats complete.cases
#' @importFrom stats median
#' @importFrom zoo na.approx
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom Rdpack reprompt
#'
#' @examples
#' data(adv)
#' res <- elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date)
#' summary(res)
#'
#' # with custom k
#' data(adv2)
#' table(adv2$intensity)
#'
#' myks <- list(displace = 20, fight = 200)
#' res <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date,
#'                k = myks, intensity = adv2$intensity)
#' extract_elo(res)
#' summary(res)
#'
#' # with custom start values
#' # if we know prior ranks:
#' myranks <- 1:7
#' names(myranks) <- letters[1:7]
#' mypriors <- createstartvalues(myranks, shape = 0.3)
#' res <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date,
#'                k = myks, intensity = adv2$intensity, startvalue = mypriors$res)
#' extract_elo(res)
#'
#' # compare elo.seq and fastelo
#' xdata <- randomsequence(10, 500)
#' allids <- colnames(xdata$pres)[2:ncol(xdata$pres)]
#' winner <- xdata$seqdat$winner
#' loser <- xdata$seqdat$loser
#' Date <- xdata$seqdat$Date
#' k <- rep(100, length(winner))
#' svals <- rep(1000, length(allids))
#'
#' res1 <- fastelo(WINNER = winner, LOSER = loser, ALLIDS = allids, KVALS = k,
#'                 STARTVALUES = svals, NORMPROB = TRUE)$ratings
#' names(res1) <- allids
#' res1 <- sort(res1, decreasing = TRUE)
#' res2 <- extract_elo(elo.seq(winner = winner, loser = loser, Date = Date,
#'                             startvalue = 1000, k = 100, normprob = TRUE,
#'                             runcheck = FALSE))
#' res1
#' res2
#' @export

elo.seq <- function(winner, loser, Date, draw = NULL, presence = NULL,
                    startvalue = 1000, k = 100, normprob = TRUE,
                    init = "average", intensity = NULL, iterate = 0,
                    runcheck = TRUE, progressbar = FALSE) {

  if (runcheck) {
    rc <- seqcheck(winner, loser, Date, draw, presence)
    if (sum(rc$checksum[c("IDcheck", "selfinteractions", "startpresence1", "startpresence2", "endpresence1", "endpresence2", "IDmatch", "IA_presencematch", "presenceentries", "datecol", "length", "continouspres", "seqdateorder")]) > 0) stop("there appear to be some problems with your data, please consider running 'seqcheck()'\notherwise set runcheck=FALSE")
  }

  # handle startvalues
  if(length(startvalue) == 1) svl <- startvalue else svl <- NA

  # save original k
  ok <- k
  # attribute k values to different intensities if specified
  if (is.list(k) & is.null(intensity)) stop("no intensity column found, or no k-list supplied")

  if (is.list(k) & !is.null(intensity)) {
    k <- as.numeric(unlist(k)[match(intensity, names(k))])
  } else {
    k <- rep(k, length(winner))
  }

  # IDs as character strings and dates as proper dates
  winner <- as.character(winner)
  loser  <- as.character(loser)
  Date   <- as.Date(as.character(Date))
  # check whether there is a column with draw/tie information
  if (is.null(draw)) {
    draw <- rep(FALSE, length(Date))
  } else {
    draw <- as.logical(as.character(draw))
  }

  # working with dates as integers (first date in sequence is set to 1) (more convenient?)
  ndat <- as.numeric(Date - min(Date) + 1)
  # keep dates for faster referencing (will not be used here but will be part of the output...)
  truedates <- seq(min(Date), max(Date), by = "day")
  # all IDs present in the data
  allids <- unique(c(winner, loser))

  # make sure that startvalues are sorted in the same way as allids
  if (length(startvalue) > 1) {
    if (length(startvalue) != length(allids)) stop("number of start values not matching number of individuals")
    startvalue <- startvalue[allids]
  } else {
    startvalue <- rep(startvalue, length(allids))
    names(startvalue) <- allids
  }

  # matrix with all dates (rows) and all ids (columns) (no rownames or special date-column set since working with numeric dates ('ndat'))
  mat <- matrix(nrow = max(ndat), ncol = length(allids))
  colnames(mat) <- allids

  ###################################
  #--- formatting presence input ---#
  #------------ START --------------#
  ###################################

  # first case, i.e. if nothing is supplied, assume that all individuals were present at all times
  if (is.null(presence)) {
    pmat <- mat; pmat[, ] <- 1
  } else {
    # presence is supplied as presence matrix (i.e. actually a data.frame...)
    if (nrow(mat) == nrow(presence)){
      pmat <- presence[, allids]
    } else {
      if (nrow(presence) < nrow(mat)) {
        stop("") #presence has fewer lines than date range of interaction sequence
      } else {
        if (nrow(presence) > nrow(mat)) {
          if ("Date" %in% colnames(presence) == FALSE) {
            stop("") #your presence data goes beyond the interaction date range, but lacks a date column (named 'Date')
          } else {
            # if presence data goes beyond date range, cut the presence data accordingly
            mindate <- min(truedates)
            maxdate <- max(truedates)
            pmat <- presence[which(as.Date(as.character(presence$Date)) == mindate) : which(as.Date(as.character(presence$Date)) == maxdate), allids]
          }
        }
      }
    }
  }

  # make the function stop if there is anything else than 0 and 1 in the matrix
  if (sum(apply(pmat, 2, function(x) (sum(is.na(x))))) > 0) stop("")
  if (sum(apply(pmat, 2, function(x) (sum(x != 0 & x != 1, na.rm = TRUE))))) stop("")
  ###################################
  #--- formatting presence input ---#
  #------------- END ---------------#
  ###################################

  ###########################
  #--- create log tables ---#
  #--------- START ---------#
  ###########################

  # create the old log table (in a different layout)
  logtable <- data.frame(Date = ndat, winner, loser,
                         Apre = as.numeric(NA), Bpre = as.numeric(NA),
                         Apost = as.numeric(NA), Bpost = as.numeric(NA),
                         draw = draw)
  if (!is.null(intensity)) logtable$intensity <- intensity

  # temporary elo log
  tempelo <- matrix(ncol = length(allids), nrow = 4, NA, dimnames = list(c("recelo", "present", "firstIA", "firstpres"), allids))
  # create matrix with number of observed interactions per day
  nmat <- mat
  nmat[, ] <- 0
  # for winners
  tabx <- table(Date, winner)
  nmat[as.character(truedates) %in% rownames(tabx), colnames(tabx)] <- nmat[as.character(truedates) %in% rownames(tabx), colnames(tabx)] + as.matrix(tabx)
  # for losers
  tabx <- table(Date, loser)
  nmat[as.character(truedates) %in% rownames(tabx), colnames(tabx)] <- nmat[as.character(truedates) %in% rownames(tabx), colnames(tabx)] + as.matrix(tabx)
  # fill ratings of those individuals that were present at the beginning (i.e. the date of the first interaction/first day of date range) with the startvalue
  startIDs <- colnames(pmat)[pmat[1, ] == 1]
  tempelo["recelo", allids] <- startvalue[allids]

  tempelo["firstpres", startIDs] <- 1

  for (m in startIDs) tempelo["firstIA", m] <- ndat[min(which(winner == m | loser == m))]
  rm(m)
  # immigrants and their first presence date and first interaction
  imiIDs <- allids[-c(which(allids %in% startIDs))]
  if (length(imiIDs) > 0) {
    imiIDs <- matrix(ncol = length(imiIDs), nrow = 2, 0, dimnames = list(c("firstIA", "firstpres"), imiIDs))
    for (m in colnames(imiIDs)) {
      imiIDs[1, m] <- ndat[min(which(winner == m | loser == m))]
    }
    rm(m)

    # special case if there is only one imigrant in the data set
    if (ncol(imiIDs) == 1) {
      imiIDs[2, ] <- min(which(pmat[, colnames(imiIDs)] == 1))
      tempelo[3:4, colnames(imiIDs)] <- imiIDs
    }

    if (ncol(imiIDs) > 1) {
      imiIDs[2, ] <- apply(pmat[, colnames(imiIDs)], 2, function(x) min(which(x == 1)) )
      # sorting
      imiIDs <- imiIDs[, names(sort(imiIDs[1, ]))]
      imiIDs <- imiIDs[, names(sort(imiIDs[2, ]))]
      tempelo[3:4, colnames(imiIDs)] <- imiIDs
    }
  }

  tempelo <- tempelo[, names(sort(tempelo[3, ], na.last = TRUE))]
  tempelo <- tempelo[, names(sort(tempelo[4, ], na.last = TRUE))]
  # reorder matrices (to fit same order of names of tempelo...)
  mat <- mat[, colnames(tempelo)]
  pmat <- pmat[, colnames(tempelo)]
  # just checking whether first interaction occurred before first presence...
  if (min(tempelo[3, ] - tempelo[4, ]) < 0) {
    xx <- paste(names(which( (tempelo[3, ] - tempelo[4, ]) < 0) ), collapse = ", ")
    stop(c("for ID (", xx, ") the first interaction occurred before first presence"))
  }
  #}

  ###########################
  #--- first interaction ---#
  #-------- START ----------#
  ###########################

  # short versions for some objects (can maybe be removed later...)
  W <- winner[1]
  L <- loser[1]
  D <- ndat[1]

  # the next two lines could be changed to refer to recelo but they might as well not...
  we <- startvalue[W] # most recent rating of winner, in this case = startingvalue
  le <- startvalue[L] # most recent rating of loser, in this case = startingvalue

  # calculate new ratings accounting for whether the interaction ended in a draw
  if (draw[1]) {
    newrat <- e.single(we, le, outcome = 0, k = k[1], normprob = normprob)
  } else {
    newrat <- e.single(we, le, outcome = 1, k = k[1], normprob = normprob)
  }

  # insert them in the matrix
  mat[1, c(W, L)] <- newrat
  # fill the logtable as well
  logtable[1, 4:7] <- c(we, le, newrat)
  # fill the recent ratings
  tempelo["recelo", c(W, L)] <- newrat

  ###########################
  #--- first interaction ---#
  #--------- END -----------#
  ###########################

  #############################################
  #--- loop through remaining interactions ---#
  #---------------- START --------------------#
  #############################################

  # loop for the remaining interactions seperated by init-type:
  log.entry.bottom <- cbind(1, "", "")
  if (init == "bottom_low") {
    # alle Tiere die am anfang 1000 bekamen bekommen nach der 1. IA wenn sie nicht interagiert haben das minimum
    id.im.min <- names(which(tempelo["recelo", ] == startvalue))
    min.rec.elo <- min(tempelo[1, ], na.rm = TRUE)
    tempelo["recelo", id.im.min] <- min.rec.elo
    mat[D, id.im.min] <- min.rec.elo
    log.entry.bottom <- cbind(1, "", paste(id.im.min,collapse = ", "))
  }

  if (init == "bottom_low" | init == "bottom") {
    # progress bar
    if (progressbar) {
      print("loop 1: Elo calculations")
      progbar <- txtProgressBar(min = 0, max = length(winner), style = 3, char = ".")
    }

    for (i in 2:length(ndat)) {
      if (progressbar) setTxtProgressBar(progbar, i)
      # grab minimum and who has the minimum
      min.rec.elo <- min(tempelo[1, ], na.rm = TRUE)
      # short versions for some objects (can maybe be removed later...)
      W <- winner[i]; L <- loser[i]; D <- ndat[i]
      # update tempelo with immigrants indication
      tempelo[2, ][colnames(tempelo) %in% names(pmat)[pmat[D, ] == 1]] <- D
      # if at least one ID is present for the FIRST time: insert a the respective BOTTOM value
      if(length(which(tempelo["firstpres", ] <= D & is.na(tempelo["recelo", ]))) > 0) {
        id.f <- names(which(tempelo["firstpres", ] <= D & is.na(tempelo["recelo", ])))
        tempelo["recelo", id.f] <- min.rec.elo
        mat[D, id.f] <- min.rec.elo
      }
      if (!exists("id.f")) id.f <- ""
      # current ratings
      we <- tempelo["recelo", W]; le <- tempelo["recelo", L]
      # calculate new ratings accounting for whether the interaction ended in a draw
      if (draw[i]) {
        newrat <- e.single(we, le, outcome = 0, k = k[i], normprob = normprob)
      } else {
        newrat <- e.single(we, le, outcome = 1, k = k[i], normprob = normprob)
      }

      # insert them in the matrix
      mat[D, c(W, L)] <- newrat
      # insert them in the recentelo
      tempelo["recelo", c(W, L)] <- newrat
      # fill the logtable as well
      logtable[i, 4:7] <- c(we, le, newrat)
      # all noninteracting animals and previously lowest rankers get the lowest updated lowest rank
      who.min.rec.elo <- names(which(tempelo[1, ] == min.rec.elo))
      min.rec.elo <- min(tempelo[1, ], na.rm = TRUE)
      id.min <- setdiff(who.min.rec.elo, c(W, L))
      tempelo[1, id.min] <- min.rec.elo
      mat[D, id.min] <- min.rec.elo
      if (!exists("id.min")) id.min <- ""
      log.entry.bottom <- c(log.entry.bottom, cbind(D, paste(id.f, collapse = ", "), paste(id.min, collapse = ", ")))
      rm(id.f, id.min)
    }
    log.entry.bottom <- as.data.frame(matrix(log.entry.bottom, ncol = 3, nrow = length(ndat), byrow = TRUE))
    names(log.entry.bottom) <- c("Date", "new.entry", "set.to.bottom.value")
  }
  # 'bottom section' END

  if (init == "average") {
    # progress bar
    if (progressbar) {
      print("loop 1: Elo calculations")
      progbar <- txtProgressBar(min = 0, max = length(winner), style = 3, char = ".")
    }

    log.entry.bottom <- cbind(1, "")
    for (i in 2:length(ndat)) {
      if (progressbar) setTxtProgressBar(progbar, i)
      # short versions for some objects (can maybe be removed later...)
      W <- winner[i]; L <- loser[i]; D <- ndat[i]
      # update tempelo with immigrants indication
      tempelo[2, ][colnames(tempelo) %in% names(pmat)[pmat[D, ] == 1]] <- D
      # if at least one ID is present for the FIRST time: insert a the respective AVERAGE value
      if (length(which(tempelo["firstpres", ] <= D & is.na(tempelo["recelo", ]))) > 0) {
        avg.elo <- round(mean(tempelo["recelo", tempelo["present", ] <= D], na.rm = TRUE))
        id.f <- names(which(tempelo["firstpres", ] <= D & is.na(tempelo["recelo", ])))
        tempelo["recelo", id.f] <- avg.elo
        mat[D, id.f] <- avg.elo
      }
      if (!exists("id.f")) id.f <- ""
      # current ratings
      we <- tempelo["recelo", W]; le <- tempelo["recelo", L]
      # calculate new ratings accounting for whether the interaction ended in a draw
      if (draw[i]) {
        newrat <- e.single(we, le, outcome = 0, k = k[i], normprob = normprob)
      } else {
        newrat <- e.single(we, le, outcome = 1, k = k[i], normprob = normprob)
      }

      # insert them in the matrix
      mat[D, c(W, L)] <- newrat
      # insert them in the recentelo
      tempelo["recelo", c(W, L)] <- newrat
      # fill the logtable as well
      logtable[i, 4:7] <- c(we, le, newrat)
      log.entry.bottom <- c(log.entry.bottom, cbind(D, paste(id.f, collapse = ", ")))
      rm(id.f)
    }
    log.entry.bottom <- as.data.frame(matrix(log.entry.bottom, ncol = 2, nrow = length(ndat), byrow = TRUE))
    names(log.entry.bottom) <- c("Date", "new.entry")
  }
  # 'average section' END

  if (progressbar) close(progbar)
  #############################################
  #--- loop through remaining interactions ---#
  #----------------- END ---------------------#
  #############################################

  ################################
  #--- matrix fill LARS style ---#
  #---------- START -------------#
  ################################

  # start with the original rating matrix
  lmat <- rbind(rep(NA, ncol(mat)), mat)
  lmat[1, startIDs] <- startvalue[startIDs]
  # fill ratings with an ID's rating from the day before if on a given day it is NA
  for (i in 2:nrow(lmat)){
    lmat[i, is.na(lmat[i, ])] <- lmat[i - 1, is.na(lmat[i, ])]
  }
  # remove ratings on days which IDs were not present (based on presence data)
  lmat <- lmat[-1, ]
  lmat[pmat == 0] <- NA

  ################################
  #--- matrix fill LARS style ---#
  #----------- END --------------#
  ################################

  ####################################
  #--- matrix fill CHRISTOF style ---#
  #------------ START ---------------#
  ####################################

  # start with the original rating matrix
  cmat <- mat
  # first: for those IDs that were present at the beginning (startIDs): backfill the first rating back up to day 1
  needtobefilled <- names(which(is.na(mat[1, startIDs])))
  if (length(needtobefilled) > 0) {
    for (ID in needtobefilled) {
      cmat[1, ID] <- lmat[min(which(complete.cases(lmat[, ID]))), ID]
    }
  }

  # which IDs were observed only at first day
  only.f <- as.numeric(which(apply(mat, 2, function(x) (length(unique(x)))) == 2 & apply(mat, 2, function(x) (!is.na(x[1])))))
  # fill ratings with an ID's rating from the day before if on a given day it is NA
  if (length(only.f) > 0){
    for (i in only.f) {
      for (j in 2:nrow(cmat)){
        cmat[j, i] <- cmat[j - 1, i]
      }
    }
  }
  # which IDs were observed (or 'guessed') at least twice
  morethan1 <- as.numeric(which(apply(lmat, 2, function(x) (length(unique(x)))) > 2) )
  for (i in morethan1) {
    cmat[, i] <- round(na.approx(cmat[, i], rule = 2))
  }
  cmat[pmat == 0] <- NA

  ####################################
  #--- matrix fill CHRISTOF style ---#
  #------------- END ----------------#
  ####################################

  ################################
  #--- stability calculations ---#
  #----------- START ------------#
  ################################

  # subfunction to calculate standardized ratings, used for weighing the stability index
  # standardized ratings range between 0 (for the individual with the lowest rating) and 1 (for the individual with the highest rating on the given day)
  elo.stdz <- function(ratings) {
    ratings <- ratings - min(ratings, na.rm = TRUE)
    return(ratings / max(ratings, na.rm = TRUE))
  }

  # get the IDs of all individuals present in the data (and the total number of IDs)
  # create empty vectors for the three variables of interest
  rankdiffs <- c(); Idspresent <- c(); eloweights <- c()

  # progress bar
  if (progressbar) {
    print("loop 2: Stability calculations")
    progbar <- txtProgressBar(min = 0, max = nrow(cmat), style = 3, char = ".")
  }

  # this loop calculates Ci for each day (except for the first one)
  for (u in 2:nrow(cmat)) {
    if (progressbar) setTxtProgressBar(progbar, u)
    # calculates the ranks the day before the actual day
    r1 <- rank(cmat[u - 1, ] * (-1), na.last = NA, ties.method = "average")
    # calculates the ranks on the test day
    r2 <- rank(cmat[u, ]   * (-1), na.last = NA, ties.method = "average")
    # which IDs were present on both days
    present <- c(names(r1), names(r2))[duplicated(c(names(r1), names(r2)))]
    # if one animal leaves, the index increases the ranks of all individuals below, i.e. if no other rank change occurs, the rankdifference will be zero in such a case
    if (length(which(!names(r1) %in% names(r2))) > 0) {
      leavers <- names(r1)[which(!names(r1) %in% names(r2))]
      for (n in 1:length(leavers)) {
        r1[which(r1 > r1[leavers[n]])] <- r1[which(r1 > r1[leavers[n]])] - 1
      }
      r1 <- r1[-c(which(names(r1) %in% leavers))]
      rm(leavers)
    }
    # calculate the weights of change (if there is none, the weight is '0')
    standardratings <- elo.stdz(cmat[u - 1, present])
    changers <- r1[r1[present] != r2[present]]
    stabweight <- 0
    if (length(changers) > 0) {
      stabweight <- as.numeric(standardratings[names(changers)[changers == min(changers)][1]])
      rm(changers)
    }

    # calculate the sum of the absolute differences in the two rankings
    rankdiffs <- c(rankdiffs, sum(abs(r2[present] - r1[present])))
    # how many individuals were present on both days
    Idspresent <- c(Idspresent, length(present))
    # the standardized elo rating of the highest rated individual involved in a rank change
    eloweights <- c(eloweights, stabweight)
    rm(stabweight, present, r2, r1)
  } # end of loop through dailyratingmatrix ('Christof - style')

  dte <- seq(from = as.Date(min(Date)), to = as.Date(max(Date)), by = 1)
  stability <- data.frame(date = dte[2:length(dte)], Idspresent, rankdiffs, eloweights)

  if (progressbar) close(progbar)

  ################################
  #--- stability calculations ---#
  #------------ END -------------#
  ################################

  ###########################
  #--- final log data... ---#
  #-------- START ----------#
  ###########################

  # store start values and k values to be included in function output
  outk <- k
  if (length(unique(startvalue)) > 1) {
    outstart <- startvalue
  } else {
    outstart <- rep(svl, length(allids))
    names(outstart) <- allids
  }

  # get some more 'log' data...

  if (length(unique(startvalue)) > 1) startvalue <- paste("custom values ranging between", range(startvalue)[1], "and", range(startvalue)[2])
  if (!is.na(svl)) startvalue <- 1000
  if (is.list(ok)) {
    k <- paste("custom values ranging between", range(unlist(ok))[1], "and", range(unlist(ok))[2])
  } else {
    k <- ok
  }


  misc <- matrix(c("init", init,
                   "k", k,
                   "startvalue", startvalue,
                   "minDate", as.character(as.Date(min(Date))),
                   "maxDate", as.character(as.Date(max(Date))),
                   "nID", length(table(c(winner, loser))),
                   "IAmin", min(table(c(winner, loser))),
                   "IAmax", max(table(c(winner, loser))),
                   "IAmean", round(mean(table(c(winner, loser))), 1),
                   "IAmedian", round(median(table(c(winner, loser))), 1),
                   "nIA", length(winner),
                   "IAperDay", round(mean(rowSums(nmat) / 2), 1),
                   "draws", round(sum(draw) / length(draw), 2),
                   "normprob", as.numeric(normprob)
  ), ncol = 2, byrow = TRUE)
  rownames(misc) <- misc[, 1]; misc <- misc[, 2]

  ###########################
  #--- final log data... ---#
  #--------- END -----------#
  ###########################

  if (length(imiIDs) == 0) {
    logtable <- logtable
  } else {
    if (init == "average") {
      logtable <- data.frame(logtable, new.entry = log.entry.bottom[, -1])
    } else {
      logtable <- data.frame(logtable, log.entry.bottom[, -1])
    }
  }
  # return and 'class' results
  res <- list(mat = mat, lmat = lmat, cmat = cmat, pmat = pmat, nmat = nmat,
              logtable = logtable, stability = stability,
              truedates = truedates, misc = misc, allids = sort(allids),
              kvals = outk, startvalues = outstart, rtype = "elo.seq"
              )

  class(res) <- "elo"
  return(res)
}

