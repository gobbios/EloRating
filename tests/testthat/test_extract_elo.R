
xdata <- randomsequence(nID = 20, avgIA = 50, presence = c(0.5, 0.4))
domdata <- xdata$seqdat
presdata <- xdata$pres
res <- elo.seq(winner = domdata$winner, loser = domdata$loser, Date = domdata$Date, presence = presdata)

# sample one individual
i <- sample(res$allids, 1)


test_that("extract_elo 1", {
  expect_equal(
    res$lmat[, i],
    extract_elo(eloobject = res, extractdate = res$truedates, IDs = i)
    )
})

test_that("extract_elo 2", {
  expect_equal(
    res$cmat[, i],
    extract_elo(eloobject = res, extractdate = res$truedates, IDs = i, NA.interpolate = TRUE)
  )
})


someids <- sample(res$allids, 100, replace = TRUE)
dates <- sample(presdata$Date, size = 100, replace = TRUE)

rats1 <- numeric(length(dates))
for(k in 1:length(rats1)) {
  rats1[k] <- extract_elo(res, dates[k], IDs = someids[k])
}

rats2 <- extract_elo(res, dates, IDs = someids)

test_that("extract_elo 3", {
  expect_equal(
    rats1,
    rats2
  )
})


test_that("extract_elo 3", expect_error(extract_elo(res, "1999-01-01")))
test_that("extract_elo 4", expect_error(extract_elo(res, dates, IDs = sample(letters[1:3]))))
test_that("extract_elo 5", expect_error(extract_elo(res, dates)))





