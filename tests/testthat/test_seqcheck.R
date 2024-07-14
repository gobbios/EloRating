

test_that("rows in presence with only 0s are detected and reported", {
  data("adv")
  data("advpres")
  
  # create faulty presence data
  # one line made up zeros
  
  faultypres <- advpres
  faultypres[15, 2:8] <- 0
  adv <- adv[-15, ] # must also remove interactions on that dates
  
  x <- seqcheck(winner = adv$winner, loser = adv$loser, Date = adv$Date, presence = faultypres)
  x <- x$zeropresencerow
  xtest <- grepl("There appear to be row(s) in the presence table that", x, fixed = TRUE)
  
  expect_true(xtest)
  
  expect_error(elo.seq(winner = adv$winner, loser = adv$loser, Date = adv$Date, presence = faultypres, runcheck = TRUE))
})



