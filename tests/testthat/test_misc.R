# this file contains tests for various smaller internal functions

testres <- logical(20)
for (i in 1:20) {
  x <- randomsequence(nID = sample(4:12, 1),
                      avgIA = sample(15:30, 1),
                      presence = c(runif(1), runif(1)))
  p <- x$pres
  (n <- n_moves(p))
  temptest <- logical(length = length(n))
  for (k in seq_len(length(n))) {
    if (n[k] %% 2 == 1) {
      temptest[k] <- p[1, names(n[k])] != p[nrow(p), names(n[k])]
    } else {
      temptest[k] <- p[1, names(n[k])] == p[nrow(p), names(n[k])]
    }
  }
  testres[i] <- all(temptest)
}

test_that("movements in and out are counted correctly by n_moves", {
  expect_true(all(testres))
})
