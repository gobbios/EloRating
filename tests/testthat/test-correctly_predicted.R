# library(EloRating)
# methods(correctly_predicted)

xdata <- randomsequence(ties = 0.2)$seqdat
s1 <- elo.seq(xdata$winner, xdata$loser, xdata$Date, draw = xdata$Draw)
s2 <- elo.seq(xdata$winner, xdata$loser, xdata$Date)
s3 <- fastelo(WINNER = xdata$winner,
              LOSER = xdata$loser,
              KVALS = rep(100, nrow(xdata)),
              ALLIDS = s1$allids,
              STARTVALUES = rep(1000, length(s1$allids)))

res1 <- correctly_predicted(s1, exclude_draws = TRUE)
res2 <- correctly_predicted(s1, exclude_draws = FALSE)
res3 <- correctly_predicted(s2)
res4 <- correctly_predicted(s3)
res5 <- correctly_predicted(s2, daterange = c("2000-01-01", "2000-01-30"))




mat <- creatematrix(s2, daterange = c("2000-01-01", "2000-03-30"))
res6 <- correctly_predicted(mat)
o <- sample(colnames(mat))
res7 <- correctly_predicted(list(o, mat))

test_that("prediction works", {
  expect_equal(res3, res4)
  expect_true(res1$total < res2$total)
  expect_true(res6$prop > res7$prop)
})

