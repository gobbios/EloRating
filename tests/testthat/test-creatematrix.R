
test_that("added individuals are present in output and indeed unobserved", {
  testres <- rep(NA, 20)
  for (i in seq_along(testres)) {
    n <- sample(4:20, 1)
    s <- randomsequence(nID = n, alphabet = FALSE)
    alldataids <- c(s$seqdat$winner, s$seqdat$loser)
    addedunobserved <- sample(LETTERS, sample(7, 1))
    
    m1 <- creatematrix(winners = s$seqdat$winner, losers = s$seqdat$loser)
    m2 <- creatematrix(winners = s$seqdat$winner, losers = s$seqdat$loser,
                       addids = addedunobserved)
    
    test1 <- sum(m1) == sum(m2)
    test2 <- all(addedunobserved %in% colnames(m2))
    test3 <- all(!addedunobserved %in% colnames(m1))
    m2sums <- rowSums(m2) + colSums(m2)
    test4 <- all(m2sums[addedunobserved] == 0)
    testres[i] <- test1 && test2 && test3 && test4
  }
  expect_true(all(testres))
})
