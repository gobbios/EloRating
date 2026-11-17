# generate data
xdata <- randomsequence(20, 100)
allids <- colnames(xdata$pres)[2:ncol(xdata$pres)]
winner <- xdata$seqdat$winner
loser <- xdata$seqdat$loser
Date <- xdata$seqdat$Date
k <- rep(100, length(winner))
svals <- rep(1000, length(allids))

# fast elo, and resorting according to names of ids from elo.seq
res1 <- fastelo(WINNER = winner, LOSER = loser, ALLIDS = allids, KVALS = k, STARTVALUES = svals, NORMPROB = TRUE)[[1]]
names(res1) <- allids
res1 <- sort(res1, decreasing = TRUE)
# elo.seq
res2 <- extract_elo(elo.seq(winner = winner, loser = loser, Date = Date, startvalue = 1000, k = 100, normprob = TRUE, runcheck = FALSE))
res2 <- res2[names(res1)]

test_that("elofast and eloseq same results", {
  expect_equal(res1, res2)
})


data(adv2)
myks <- list(displace = 20, fight = 200)
res1 <- elo.seq(winner = adv2$winner, loser = adv2$loser, Date = adv2$Date, k = myks, intensity = adv2$intensity)
res1 <- extract_elo(res1)

ks <- unlist(myks[adv2$intensity])
res2 <- fastelo(WINNER = adv2$winner, LOSER = adv2$loser, ALLIDS = letters[1:7], KVALS = ks, STARTVALUES = rep(1000, 7), NORMPROB = TRUE)[[1]]
names(res2) <- letters[1:7]
res2 <- sort(res2, decreasing = TRUE)

test_that("elofast and eloseq same results with custom k", {
  expect_equal(res1, res2)
})

res1
res2


test_that("elofast handles normprob correctly", {
  w <- rep(c("a", "a", "b", "a", "d"), 2)
  l <- rep(c("b", "c", "c", "c", "a"), 2)
  o <- sample(seq_along(w))
  w <- w[o]
  l <- l[o]
  nids <- 4
  
  # win probs 1
  xx <- fastelo(WINNER = w, LOSER = l, ALLIDS = letters[seq_len(nids)], KVALS = rep(100, length(w)), STARTVALUES = rep(1000, nids), NORMPROB = TRUE)
  xx$ratings
  yy <- elo.seq(w, l, Date = as.Date(1:length(w)), normprob = TRUE)
  yy$lmat[nrow(yy$lmat), ]
  
  
  xtest <- as.numeric(xx$winprobs) - winprob(yy$logtable$Apre, yy$logtable$Bpre, normprob = TRUE) 
  expect_all_equal(object = round(xtest, 6), expected = 0)
  
  
  # win probs 2
  xx <- fastelo(WINNER = w, LOSER = l, ALLIDS = letters[seq_len(nids)], KVALS = rep(100, length(w)), STARTVALUES = rep(1000, nids), NORMPROB = FALSE)
  xx$ratings
  yy <- elo.seq(w, l, Date = as.Date(1:length(w)), normprob = FALSE)
  yy$lmat[nrow(yy$lmat), ]
  xtest <- as.numeric(xx$winprobs) - winprob(yy$logtable$Apre, yy$logtable$Bpre, normprob = FALSE) 
  expect_all_equal(object = round(xtest, 6), expected = 0)
  
})

