
data(adv)
SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date, progressbar = F)

test_that("data supplying in creatematrix", {
  expect_error(creatematrix(winners=adv$winner, losers = adv$loser, eloobject = SEQ))
  expect_error(creatematrix(winners=adv$winner))
})

