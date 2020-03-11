# create a simple matrix with 1 inconsistency
mat <- matrix(rep(0, 25), ncol = 5)
colnames(mat) <- rownames(mat) <- LETTERS[1:5]
mat[1, 2:5] <- 1
mat[2, 3] <- 1
mat[3, 4] <- 1
mat[4, 2] <- 1


test_that("ISI returns a list", {
  expect_message(res <- ISI(mat = mat, runs = 5000, printmessages = TRUE))
  expect_true(length(res) > 1)
})

res <- ISI(mat = mat, runs = 50, printmessages = FALSE)
test_that("ISI ranking contains at least one ranking", {
  expect_true(ncol(ISIranks(res)) == length(res) + 2)
})



# .diffmat
n <- sample(5:20, 1)
mat2 <- matrix(ncol = n, nrow = n)
res <- EloRating:::.diffmat(mat2)
res <- res[upper.tri(res)] + t(res)[upper.tri(res)]

test_that("helper functions work", {
  expect_true(length(table(res)) == 1 & res[1] == n + 1)
  expect_true(EloRating:::.incon(mat) == 1)
  expect_true(EloRating:::.sincon(mat) == 2)
  expect_true(nrow(incontable(mat)) == 1)
})


data("devries98")
xres <- sapply(1:10, function(X)ISI(devries98, 20, printmessages = FALSE)[1])
xres <- lapply(xres, colSums)
xres <- lapply(xres, function(X)X[colnames(devries98)])
xres <- unlist(lapply(xres, function(X)all(colSums(devries98) == X)))

test_that("putting back of non-zero ties works", {
  expect_true(all(xres))
})
