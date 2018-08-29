# generate data
xdata <- randomsequence(20, 100)
xdata$seqdat$intensity <- sample(c("fight", "displace"), nrow(xdata$seqdat), replace = TRUE)
elores <- elo.seq(winner = xdata$seqdat$winner, loser = xdata$seqdat$loser, Date = xdata$seqdat$Date,
                  intensity = xdata$seqdat$intensity, runcheck = FALSE)


res1 <- optimizek(eloobject = elores, krange = c(2, 400), optimode = "loop", resolution = 15, doplot = TRUE)
res2 <- optimizek(eloobject = elores, krange = c(2, 400), optimode = "loopfast", resolution = 15, doplot = TRUE)

test_that("optimal k one-dimension", {
  expect_equal(res1, res2)
})


klist <- list(fight = c(100, 400), displace = c(50, 350))
res1 <- optimizek(eloobject = elores, krange = klist, optimode = "loop", resolution = 10)
res2 <- optimizek(eloobject = elores, krange = klist, optimode = "loopfast", resolution = 10)

test_that("optimal k two dimensions", {
  expect_equal(res1, res2)
})
