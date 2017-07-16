test_that("e.single output", {
  expect_equal(e.single(1000, 1500, 1, normprob = T), c(1096, 1404))
  expect_equal(e.single(1000, 1500, 1, normprob = F), c(1095, 1405))
})
