
# init settings ---------------
context("init = bottom performs properly")
testres <- matrix(ncol = 2, nrow = 100, NA)

for (i in seq_len(nrow(testres))) {
  x <- randomsequence(nID = sample(4:12, 1),
                      avgIA = sample(15:30, 1),
                      presence = c(runif(1), runif(1)))

  sdat <- x$seqdat
  pres <- x$pres
  # sometimes the random sequence generator acts up and produces very spotty
  # presence files (only one id present on some days):
  # make sure those are not processed
  step1 <- unlist(apply(pres[, 2:5], 2, function(x)sum(diff(x) != 0)))
  if (!any(step1 > 2)) {
    if ((ncol(pres) - 1) == length(unique(c(sdat$winner, sdat$loser)))) {
      if (any(pres[1, 2:ncol(pres)] == 0)) {
        res <- elo.seq(winner = sdat$winner,
                       loser = sdat$loser,
                       Date = sdat$Date,
                       presence = pres,
                       init = "bottom")

        test_id <- which(pres[1, ] == 0)[1]
        test_id <- colnames(pres)[test_id]

        xlog <- res$logtable
        m <- min(which(xlog$winner == test_id), which(xlog$loser == test_id))
        xlog[m, ]
        sord <- which(xlog[m, c("winner", "loser")] == test_id)
        startvalue <- xlog[m, c("Apre", "Bpre")[sord]]
        testres[i, 1] <- min(res$mat[m - 1, ], na.rm = TRUE) == startvalue
        testres[i, 2] <- sum(pres[1, 2:ncol(pres)] == 0) > 1
        if (!testres[i, 1]) stop()
      }
    }
  }
}

test_that("init = bottom performs properly", {
  expect_true(all(na.omit(testres[, 1])))
})
