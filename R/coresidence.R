#' coresidence summary
#'
#' @param eloobject result from \code{\link{elo.seq}}
#'
#' @details This function provides a summary of the presence of individuals (and dyads) during the data sequence. This will be only informative if there was actually presence information supplied to \code{elo.seq}.
#'
#' @seealso \code{\link{presence_summary}}
#'
#' @return a list with three items:
#' \describe{
#'  \item{\code{$global} (a numeric vector)}{
#'    \describe{
#'    \item{\code{n_int}}{total number of interactions}
#'    \item{\code{n_dyads}}{total number of dyads}
#'    \item{\code{prop_nocores}}{proportion of dyads that were never co-resident}
#'    \item{\code{mean_cores_prop}}{mean proportion over individuals of proportions of all other IDs the focal was co-resident with at some point}
#'    }
#'  }
#'  \item{\code{$dyads} (a data.frame)}{
#'    \describe{
#'    \item{\code{id1, id2}}{the IDs}
#'    \item{\code{n_int}}{number of interactions for dyad}
#'    \item{\code{cores_dur}}{the duration of co-residence}
#'    \item{\code{none_dur}}{the duration for neither ID being present (both are absent)}
#'    \item{\code{one_dur}}{the duration of time when one ID was present but not the other}
#'    }
#'  }
#'  \item{\code{$individuals} (a data.frame)}{
#'    \describe{
#'    \item{\code{id}}{the ID}
#'    \item{\code{n_int}}{number of interactions}
#'    \item{\code{presdays}}{days of presence}
#'    \item{\code{cores_n_ind}}{co-resident with these individuals at some point}
#'    \item{\code{cores_prop}}{proportion of individuals with which ID was co-resident}
#'    \item{\code{stints}}{number of continuous bouts of presence}
#'    }
#'  }
#' }
#' @export
#'
#' @examples
#' set.seed(123)
#' IA <- randomsequence(nID = 10, avgIA = 20, presence = c(0.7, 0.8))
#' SEQ <- elo.seq(winner = IA$seqdat$winner, loser = IA$seqdat$loser, Date = IA$seqdat$Date,
#'                presence = IA$pres, runcheck = FALSE, progressbar = FALSE)
#' coresidence(SEQ)

coresidence <- function(eloobject) {
  # make dyads
  allids <- sort(eloobject$allids)
  dyads <- t(combn(allids, 2))
  colnames(dyads) <- c("id1", "id2")

  cores_dur <- numeric(nrow(dyads))
  none_dur <- numeric(nrow(dyads))
  n_int <- numeric(nrow(dyads))

  # presence matrix
  pmat <- as.matrix(eloobject$pmat)

  # co-residence for each dyad
  # would probably be faster with 'apply' but clearer this way...
  for (i in seq_len(nrow(dyads))) {
    cores_dur[i] <- sum(pmat[, dyads[i, "id1"]] + pmat[, dyads[i, "id2"]] == 2)
    none_dur[i] <- sum(pmat[, dyads[i, "id1"]] + pmat[, dyads[i, "id2"]] == 0)
  }

  # number of interactions
  ints <- cbind(as.character(eloobject$logtable$winner),
                as.character(eloobject$logtable$loser))
  ints <- t(apply(ints, 1, sort))
  ints <- data.frame(as.table(creatematrix(winners = ints[, 1],
                                           losers = ints[, 2])))
  ints <- ints[ints[, 3] > 0, ]
  for (i in seq_len(nrow(ints))) {
    x <- which(dyads[, "id1"] == ints$Var1[i] & dyads[, "id2"] == ints$Var2[i])
    n_int[x] <- ints$Freq[i]
    rm(x)
  }
  # sanity check
  if (sum(n_int) != nrow(eloobject$logtable)) stop("sanity check 1 failure")

  # combine results
  dyad_res <- data.frame(id1 = dyads[, "id1"],
                         id2 = dyads[, "id2"],
                         n_int = n_int,
                         cores_dur = cores_dur,
                         none_dur = none_dur)
  dyad_res$one_dur <- nrow(pmat) - cores_dur - none_dur
  # sanity check
  x <- rowSums(dyad_res[, c("cores_dur", "none_dur", "one_dur")])
  if (length(unique(x)) != 1) stop("sanity check 2 failure")

  # summarize by individual
  idres <- data.frame(id = allids, stringsAsFactors = FALSE)
  idres$n_int <- NA
  idres$presdays <- NA
  idres$cores_n_ind <- NA
  idres$cores_prop <- NA
  idres$stints <- NA

  for (i in seq_len(nrow(idres))) {
    xlines <- apply(dyads, 1, function(X) idres$id[i] %in% X)
    temp <- dyad_res[xlines, ]
    idres$n_int[i] <- sum(temp$n_int)
    idres$presdays[i] <- sum(pmat[, idres$id[i]])
    # coresident with how many of the others
    idres$cores_n_ind[i] <- sum(temp$cores_dur > 0)
    idres$cores_prop[i] <- idres$cores_n_ind[i] / nrow(temp)
    # number of stints
    if (sum(pmat[, idres$id[i]]) == 0) {
      idres$stints[i] <- 0
    } else {
      idres$stints[i] <- sum(diff(which(pmat[, idres$id[i]] == 1)) != 1) + 1
    }
  }
  # sanity check
  if (sum(idres$n_int)/2 != nrow(eloobject$logtable)) stop("sanity check 3 failure")
  if (sum(idres$presdays) != sum(pmat)) stop("sanity check 4 failure")

  # some global results
  glob_res <- c(n_int = nrow(eloobject$logtable), n_dyads = nrow(dyad_res))
  # proportion of dyads that were never coresident
  glob_res <- c(glob_res, prop_nocores = sum(dyad_res$cores_dur == 0) / nrow(dyad_res))
  # mean proportion of coresidency with all other ids
  glob_res <- c(glob_res, mean_cores_prop = mean(idres$cores_prop))

  return(list(global = glob_res, dyads = dyad_res, individuals = idres))
}
