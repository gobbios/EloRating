
data("bonobos")
temp <- DS(bonobos, prop = "Pij")

ores1 <- c(14.01, 11.64, 6.61, -1.29, -5.99, -8.73, -16.25)
ores2 <- c(5.00, 4.66, 3.94, 2.82, 2.14, 1.75, 0.68)
DSres1 <- round(temp$DS, 2)
DSres2 <- round(temp$normDS, 2)

test_that("DS output", {
  expect_equal(ores1, DSres1)
  expect_equal(ores2, DSres2)
})
