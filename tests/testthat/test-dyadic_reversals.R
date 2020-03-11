xdata <- randomsequence(nID = sample(5:20, 1),
                        avgIA = sample(5:20, 1),
                        ties = runif(1, 0, 0.2),
                        reversals = runif(1, 0.05, 0.3))$seqdat
# create some date heterogeneity
xdata$Date <- sort(sample(xdata$Date, replace = TRUE))
elores <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date,
                  draw = xdata$Draw, runcheck = FALSE, progressbar = FALSE)
res <- dyadic_reversals(elores)

test_that("all interactions are considered", {
  expect_equal(sum(res$pre_n + res$post_n), sum(!xdata$Draw))
})

test_that("relationship is NA if no interactions occured", {
  expect_true(sum(res$pre_n[is.na(res$pre)]) == 0)
  expect_true(sum(res$post_n[is.na(res$post)]) == 0)
})
