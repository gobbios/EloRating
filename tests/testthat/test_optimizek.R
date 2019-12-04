# generate data
xdata <- randomsequence(20, 100)
xdata$seqdat$intensity <- sample(c("fight", "displace"), nrow(xdata$seqdat), replace = TRUE)

# fit classic elo
elores1 <- elo.seq(winner = xdata$seqdat$winner, loser = xdata$seqdat$loser, Date = xdata$seqdat$Date,
                   intensity = xdata$seqdat$intensity, runcheck = FALSE)
# fit fastelo
elores2 <- fastelo(WINNER = xdata$seqdat$winner, LOSER = xdata$seqdat$loser, ALLIDS = colnames(xdata$pres)[2:21], KVALS = rep(100, nrow(xdata$seqdat)), STARTVALUES = rep(1000, ncol(xdata$pres)-1))

# optimize with single interaction type
res1 <- optimizek(eloobject = elores1, krange = c(2, 400), resolution = 50, doplot = TRUE)
res2 <- optimizek(eloobject = elores2, krange = c(2, 400), resolution = 50, doplot = TRUE)

test_that("optimal k one-dimension", {
  expect_equal(res1, res2)
})


# optimize with two interaction types
klist <- list(fight = c(100, 400), displace = c(50, 350))

res1 <- optimizek(eloobject = elores1, krange = klist, resolution = 50, itype = xdata$seqdat$intensity)
res2 <- optimizek(eloobject = elores2, krange = klist, resolution = 50, itype = xdata$seqdat$intensity)

test_that("optimal k two dimensions", {
  expect_equal(res1, res2)
})
