test_that("steepness values work", {
  xdata <- randomsequence()
  mat1 <- creatematrix(winners = xdata$seqdat$winner, losers = xdata$seqdat$loser)
  mat2 <- creatematrix(winners = xdata$seqdat$winner, losers = xdata$seqdat$loser, addids = c("MOC", "XYZ"))
  
  s_foo <- as.numeric(steepness(mat1, nrand = 2, Dij = TRUE)["steep"])
  ds <- DS(mat1, prop = "Dij")
  s_man <- -as.numeric(coef(lm(ds$normDS ~ seq_len(nrow(ds))))[2])
  expect_equal(s_foo, s_man, tolerance = 0.0001)
  
  s_foo <- as.numeric(steepness(mat1, nrand = 2, Dij = FALSE)["steep"])
  ds <- DS(mat1, prop = "Pij")
  s_man <- -as.numeric(coef(lm(ds$normDS ~ seq_len(nrow(ds))))[2])
  expect_equal(s_foo, s_man, tolerance = 0.0001)

  s_foo <- as.numeric(steepness(mat2, nrand = 2, Dij = TRUE)["steep"])
  ds <- DS(mat2, prop = "Dij")
  s_man <- -as.numeric(coef(lm(ds$normDS ~ seq_len(nrow(ds))))[2])
  expect_equal(s_foo, s_man, tolerance = 0.0001)
  
  s_foo <- as.numeric(steepness(mat2, nrand = 2, Dij = FALSE)["steep"])
  ds <- DS(mat2, prop = "Pij")
  s_man <- -as.numeric(coef(lm(ds$normDS ~ seq_len(nrow(ds))))[2])
  expect_equal(s_foo, s_man, tolerance = 0.0001)
})





test_that("steepness function does sensible things", {
  xdata <- randomsequence()
  mat1 <- creatematrix(winners = xdata$seqdat$winner, losers = xdata$seqdat$loser)
  s_foo <- steepness(mat1, nrand = 2)
  expect_true(!is.na(s_foo["p"]))
  expect_true(all(c("steep", "expected", "p", "nrand") %in% names(s_foo)))
  s_foo <- steepness(mat1, nrand = 0)
  expect_true(is.na(s_foo["p"]))
  expect_true(all(c("steep", "expected", "p", "nrand") %in% names(s_foo)))
  
  
  # plot doesn't fail?
  steepness(mat1, nrand = 50, returnfig = TRUE)
})