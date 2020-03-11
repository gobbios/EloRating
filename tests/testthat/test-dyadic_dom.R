testres1 <- rep(FALSE, 3)
testres2 <- rep(FALSE, 3)
for (i in seq_along(testres1)) {
  xdata <- randomsequence(nID = sample(5:20, 1),
                          avgIA = sample(3:20, 1),
                          reversals = runif(1))$seqdat
  dd <- dyadic_dom(xdata$winner, xdata$loser)
  # prop unknown matches
  p1 <- round(prunk(creatematrix(winners = xdata$winner,
                                 losers = xdata$loser))[1], 3)
  p2 <- round(sum(dd$id1_wins + dd$id2_wins == 0) / nrow(dd), 3)
  if (p1 == p2) testres1[i] <- TRUE
  # total number of interactions
  if (nrow(xdata) == sum(dd$id1_wins + dd$id2_wins)) {
    testres2[i] <- TRUE
  }
}

test_that("dyadic dom table is correct", {
  expect_true(all(testres1))
  expect_true(all(testres2))
})
