
data(adv)
data(advpres)
SO <- EloRating:::.elo.seq_old(winner = adv$winner,
                               loser = adv$loser,
                               Date = adv$Date,
                               k = 100,
                               startvalue = 1000,
                               presence = advpres)
SOb <- EloRating:::.elo.seq_old(winner = adv$winner,
                                loser = adv$loser,
                                Date = adv$Date,
                                k = 100,
                                startvalue = 1000,
                                presence = advpres,
                                init = "bottom")
SObl <- EloRating:::.elo.seq_old(winner = adv$winner,
                                 loser = adv$loser,
                                 Date = adv$Date,
                                 k = 100,
                                 startvalue = 1000,
                                 presence = advpres,
                                 init = "bottom_low")

# kval list produces sensical results
adv$intens <- "alwaysthesame"
kvals <- list(alwaysthesame = 100)
SM1 <- elo.seq(winner = adv$winner,
               loser = adv$loser,
               Date = adv$Date,
               intensity = adv$intens,
               k = kvals,
               startvalue = 1000,
               presence = advpres)
SM1b <- elo.seq(winner = adv$winner,
                loser = adv$loser,
                Date = adv$Date,
                intensity = adv$intens,
                k = kvals,
                startvalue = 1000,
                presence = advpres,
                init = "bottom")
SM1bl <- elo.seq(winner = adv$winner,
                 loser = adv$loser,
                 Date = adv$Date,
                 intensity = adv$intens,
                 k = kvals,
                 startvalue = 1000,
                 presence = advpres,
                 init = "bottom_low")

adv$intens <- sample(c("alwaysthesame", "another"), nrow(adv), TRUE)
kvals <- list(alwaysthesame = 100, another = 100)

SM2 <- elo.seq(winner = adv$winner,
               loser = adv$loser,
               Date = adv$Date,
               intensity = adv$intens,
               k = kvals,
               startvalue = 1000,
               presence = advpres)
SM2b <- elo.seq(winner = adv$winner,
                loser = adv$loser,
                Date = adv$Date,
                intensity = adv$intens,
                k = kvals,
                startvalue = 1000,
                presence = advpres,
                init = "bottom")
SM2bl <- elo.seq(winner = adv$winner,
                 loser = adv$loser,
                 Date = adv$Date,
                 intensity = adv$intens,
                 k = kvals,
                 startvalue = 1000,
                 presence = advpres,
                 init = "bottom_low")


test_that("differential k", {
  expect_equal(extract_elo(SO), extract_elo(SM1))
  # expect_equal(extract_elo(SOb), extract_elo(SM1b))
  expect_equal(extract_elo(SObl), extract_elo(SM1bl))
  expect_equal(extract_elo(SO), extract_elo(SM2))
  # expect_equal(extract_elo(SOb), extract_elo(SM2b))
  expect_equal(extract_elo(SObl), extract_elo(SM2bl))
})


#######################

svals <- createstartvalues(ranks = rep(1, 7), startvalue = 1000, k = 100)
names(svals$res) <- letters[1:7]
SM3 <- elo.seq(winner = adv$winner,
               loser = adv$loser,
               Date = adv$Date,
               k = 100,
               startvalue = svals$res,
               presence = advpres)
SM3b <- elo.seq(winner = adv$winner,
                loser = adv$loser,
                Date = adv$Date,
                k = 100,
                startvalue = svals$res,
                presence = advpres,
                init = "bottom")
SM3bl <- elo.seq(winner = adv$winner,
                 loser = adv$loser,
                 Date = adv$Date,
                 k = 100,
                 startvalue = svals$res,
                 presence = advpres,
                 init = "bottom_low")


test_that("custom start values", {
  expect_equal(extract_elo(SO), extract_elo(SM3))
  # expect_equal(extract_elo(SOb), extract_elo(SM3b))
  expect_equal(extract_elo(SObl), extract_elo(SM3bl))
})
