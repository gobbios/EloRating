# this is a collection of smaller helper files that are of no immediate value to
# the user

# create the tempelo object in elo.seq
make_tempelo <- function(winner, loser, pmat, allids, startvalue, Date, truedates) {
  # first interaction
  # first_ia <- sapply(allids, function(x) min(which(winner == x | loser == x)))
  
  # first interaction (at which line in presence matrix!!!)
  first_ia <- sapply(allids, function(x) min(which(winner == x | loser == x)))
  first_ia <- Date[first_ia]
  first_ia <- sapply(first_ia, function(x)which(truedates == x))
  
  # first presence
  first_pres <- apply(pmat[, allids], 2, function(x)min(which(x == 1)))
  # combine
  res <- rbind(startvalue, NA, first_ia, first_pres)
  rownames(res) <- c("recelo", "present", "firstIA", "firstpres")
  res["recelo", which(pmat[1, allids] == 0)] <- NA

  # reorder
  res <- res[, names(sort(res[3, ], na.last = TRUE))]
  res <- res[, names(sort(res[4, ], na.last = TRUE))]

  res
}

# check whether presence data has individuals that both join and leave or
# leave and join
# i.e. have more than one movement in/out the group
n_moves <- function(presence) {
  presence <- presence[, -c(which(colnames(presence) %in% c("Date", "date")))]
  colSums(apply(presence, 2, function(x) diff(x) != 0))
}

