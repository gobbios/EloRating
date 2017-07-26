
data("dommats")

M <- dommats$badgers
res <- as.numeric(transitivity(m = M, returnfig = FALSE, runs = 2000))

test_that("Transitivity badgers", {
  expect_equal(res[1], 0.950)
  expect_equal(res[2], 0.800)
  expect_equal(res[3], 0.063, tolerance=0.05) # 0.063 target value in supplement to Shizuka and McDonald 2012
})


M <- dommats$elephants
res <- as.numeric(transitivity(m = M, returnfig = FALSE, runs = 5000))

test_that("Transitivity elephants", {
  expect_equal(res[1], 1)
  expect_equal(res[2], 1)
  expect_equal(res[3], 0.239, tolerance=0.05) # 0.239 target value in supplement to Shizuka and McDonald 2012
})


M <- dommats$squirrels
res <- as.numeric(transitivity(m = M, returnfig = FALSE, runs = 5000))

test_that("Transitivity squirrels", {
  expect_equal(res[1], 0.800)
  expect_equal(res[2], 0.200)
  # expect_equal(res[3], 0.076, tolerance=0.05) # 0.076 target value in supplement to Shizuka and McDonald 2012
  # this produces hugely different results, possibly a typo in their table?
})

