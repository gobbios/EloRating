
xdata <- read.table(system.file("ex-sequence.txt", package = 'EloRating'), header = TRUE)
res <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date, runcheck = FALSE)
newres <- extract_elo(res, "2000-05-28")
# from old version/vignette (2014, p. 3f)
oldres <- c(c = 1342, d = 1214, a = 1161, f = 1133, k = 1011, s = 1000, g = 958, n = 844, w = 799, z = 538)
test_that("newold1", {
  expect_equal(newres, oldres)
})

oldres <- c(c = 1342, a = 1161, k = 1011, s = 1000)
newres <- extract_elo(res, "2000-05-28", IDs = c("s", "a", "c", "k"))
test_that("newold2", {
  expect_equal(newres, oldres)
})

# from old version/vignette (2014, p. 6f)
xpres <- read.table(system.file("ex-presence.txt", package = "EloRating"), header = TRUE)
xpres[, 1] <- as.Date(as.character(xpres[, 1]))
res2 <- elo.seq(winner = xdata$winner, loser = xdata$loser, Date = xdata$Date, presence = xpres, draw = xdata$Draw)
oldres <- c(c = 1340, d = 1211, f = 1136, a = 1092, k = 962, g = 960, n = 873, w = 860, z = 566)
newres <- extract_elo(res2, extractdate = "2000-05-28")
test_that("newold3", {
  expect_equal(newres, oldres)
})

oldres <- c(c = 1340, a = 1092, k = 962, s = NA)
newres <- extract_elo(res2, extractdate = "2000-05-28", IDs = c("s", "a", "c", "k"))
test_that("newold4", {
  expect_equal(newres, oldres)
})


# from old version/vignette (2014, p. 8)
oldres <- 0.9674
stab_elo(res2, from="2000-05-05", to="2000-06-05")


# from old version/vignette (2014, p. 8f)
newres <- traj_elo(res2, ID = c("s", "f", "n", "z"), from = "2000-05-05", to = "2000-06-05")
oldres <- c(1.696998, 3.904463)
test_that("newold6", {
  expect_equal(newres$slope[2:3], oldres, tolerance = 0.00001)
})


# from old version/vignette (2014, p. 9)
test_that("newold7", {
  expect_equal(individuals(res2, from = "2000-05-05", to = "2000-05-05", outp = "N"), 8, tolerance = 0.00001)
  expect_equal(individuals(res2, from = "2000-05-05", to = "2000-06-05", outp = "N"), 8.3125, tolerance = 0.00001)
  expect_equal(individuals(res2, from = "2000-05-05", to = "2000-06-05", outp = "CV"), 0.07125283, tolerance = 0.00001)
  expect_equal(individuals(res2, from = "2000-05-05", to = "2000-06-05", outp = "IDs"), c( "d", "k", "n", "w", "z", "c", "g", "f", "a", "s"))
})


test_that("newold8", {
  expect_equal(winprob(1000, 1200), 0.2397501, tolerance = 0.00001)
  expect_equal(winprob(1200, 1000), 0.7602499, tolerance = 0.00001)
  expect_equal(winprob(1200, 1200), 0.5, tolerance = 0.00001)
})





oldres <- matrix(c(
  14.011236, 5.0016051,
  11.635634, 4.6622334,
  6.606742, 3.9438202,
  -1.285714, 2.8163265,
  -5.988764, 2.1444623,
  -8.729133, 1.7529810,
  -16.250000, 0.6785714
), ncol = 2, byrow = TRUE)

data(bonobos)
newres <- DS(bonobos, prop = "Pij")
test_that("newold9", {
  expect_equal(newres$DS, oldres[, 1], tolerance = 0.00001)
  expect_equal(newres$normDS, oldres[, 2], tolerance = 0.00001)
})
