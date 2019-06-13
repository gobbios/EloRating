# create a random sequence
s <- randomsequence(nID = 50, avgIA = 5)[[1]]
s <- s[, c("winner", "loser")]
# create a matrix from this sequence
mat <- creatematrix(winners = s$winner, losers = s$loser)
# make sequence from matrix
newseq <- mat2seq(mat = mat)

dyads1 <- table(apply(s, 1, paste, collapse = "_"))
dyads2 <- table(apply(newseq, 1, paste, collapse = "_"))

test_that("mat2seq", {
  expect_identical(table(s$winner), table(newseq$winner))
  expect_identical(table(s$loser), table(newseq$loser))
  expect_identical(dyads1, dyads2)
})
