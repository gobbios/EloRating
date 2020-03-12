
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date, progressbar = F)

# create matrix works only of winners and losers are properly specified
test_that("data supplying in creatematrix", {
  expect_error(creatematrix(winners=adv$winner, losers = adv$loser, eloobject = SEQ))
  expect_error(creatematrix(winners=adv$winner))
})



# test whether creatematrix and mat2seq work properly
# mat -> seq -> mat
test_that("data conversion", {
  N <- sample(5:20, 1)
  mat <- matrix(ncol=N, nrow=N)
  mat[,] <- rpois(length(mat), lambda = runif(1, 0.5, 5))
  colnames(mat) <- rownames(mat) <- letters[1:N]
  S <- mat2seq(mat = mat)
  M2 <- creatematrix(winners = S$winner, losers = S$loser)
  M2 <- M2[sort(colnames(M2)), sort(colnames(M2))]
  res <- M2 - mat
  expect_equal(sum(colSums(res)+rowSums(res)), 0)
})

