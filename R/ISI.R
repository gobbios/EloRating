#' de Vries' I&SI ranking
#'
#' de Vries' I&SI ranking
#'
#' @param mat square interaction matrix with winner in rows and losers in columns, for example the output from \code{\link{creatematrix}}
#' @param runs numeric, number of iterations, by default \code{5000}
#' @param printmessages logical, should the number of I and SI be printed (as well as a message if there is more than one solution). By default \code{TRUE}.
#'
#' @return a list with the best possible matrix (or matrices if there is more than one best solution)
#'
#' @author Christof Neumann
#'
#' @references
#' \insertRef{devries1998}{EloRating}
#'
#' @details The number of interations is set substantially higher than what was suggested in the de Vries' 1998 paper, because my algorithm here is less efficient.
#'
#' The I&SI algorithm (c.f. de Vries 1998) does not necessarily result in a unique order (see example below). If such a case occurs, all (equally good) solutions are returned as a list.
#'
#' The function checks whether a \code{table} is supplied instead of a \code{matrix} and converts from table to matrix if possible (trying to keep the column and row names if supplied in the table).
#'
#' If the matrix does not have column-names, unique column- and row-names are assigned.
#'
#' @importFrom stats na.omit
#' @seealso \code{\link{ISIranks}}
#'
#' @examples
#'  data(devries98)
#'  h.index(devries98)
#'  ISI(devries98)
#'
#'  ##
#'  data(adv)
#'  SEQ <- elo.seq(winner=adv$winner, loser=adv$loser, Date=adv$Date)
#'  mat <- creatematrix(SEQ)
#'  res <- ISI(mat)
#'  # note that this matrix is not sufficiently linear to justify such ordering
#'  h.index(mat)
#'
#' @export

ISI <- function(mat, runs = 5000, printmessages = TRUE) {
  # used helper functions
  makena <- function(mat) {
    newmat <- mat
    newmat[newmat - t(newmat) == 0] <- NA
    return(newmat)
  }

  randmat <- function(mat) {
    s <- sample(seq_len(ncol(mat)))
    return(mat[s, s])
  }

  largestincon <- function(mat) {
    outmat <- matrix(FALSE, ncol = ncol(mat), nrow = nrow(mat))
    outmat[upper.tri(mat)] <- mat[upper.tri(mat)] < t(mat)[upper.tri(mat)]
    m <- matrix(seq_along(mat), ncol(mat), byrow = FALSE) + ncol(mat) - 1
    ms <- na.omit(m[outmat])
    ps <- na.omit(.diffmat(mat)[outmat])
    x <- ms[which(ps == max(ps))]
    if (length(x) > 1) x <- sample(x, 1)
    return(c(x %/% ncol(mat), x %% ncol(mat) + 1))
  }

  flip1 <- function(mat, pos) {
    s <- seq_len(ncol(mat))
    s[pos] <- rev(pos)
    return(mat[s, s])
  }

  compare2isi <- function(x, y) {
    res <- FALSE
    if (x[1] < y[1]) res <- TRUE
    if (x[1] == y[1] & x[2] < y[2]) res <- TRUE
    return(res)
  }

  laststep <- function(mat) {
    si <- .sincon(mat)
    SWITCH <- FALSE
    while (SWITCH == FALSE) {
      SWITCH <- TRUE
      for (i in 2:ncol(mat)) {
        if (mat[i, i - 1] == mat[i - 1, i]) {
          di <- di1 <- 0
          for (k in seq_len(ncol(mat))) {
            di <- di + sign(mat[i - 1, k] - mat[k, i - 1])
            di1 <- di1 + sign(mat[i, k] - mat[k, i])
          }

          if (di1 > di) {
            mat <- flip1(mat, c(i - 1, i))
            bestsi <- .sincon(mat)
            if (bestsi > si) mat <- flip1(mat, c(i - 1, i))
            if (bestsi <= si) {
              si <- bestsi
              SWITCH <- FALSE
            }
          }
        }
      }
    }
    return(mat)
  }

  # some checks:
  # require square matrix (also works for table)
  if (nrow(mat) != ncol(mat)) stop("matrix is not square")

  # convert table to matrix
  if (is.table(mat)) {
    cn <- colnames(mat)
    mat <- matrix(mat, nrow = nrow(mat))
    colnames(mat) <- rownames(mat) <- cn
    diag(mat) <- 0
  }

  # require column names
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("i", seq_len(ncol(mat)))
    rownames(mat) <- colnames(mat)
  }


  # get an index
  mindex <- seq_len(ncol(mat))
  # start with randomized matrix
  # in which unknown and tied relationships (also diagonal) are marked "NA"
  bestmat <- makena(randmat(mat))
  # get the metrics of this matrix
  bestmetrics <- c(.incon(bestmat), .sincon(bestmat))
  # create a list for candidate matrices
  res <- list()

  # set the overall counter and the get-stuck counter
  cnt <- 0
  stuck <- 0

  # start loop
  while (cnt < runs) {
    cnt <- cnt + 1
    stuck <- stuck + 1
    # create testmat (apply flipping rule) and compare with best mat
    testmat <- flip1(bestmat, largestincon(bestmat))
    # store as bestmat if better than before
    temp <- c(.incon(testmat), .sincon(testmat))
    compare2isi(temp, bestmetrics)
    if (compare2isi(temp, bestmetrics)) {
      bestmat <- testmat
      bestmetrics <- temp
      stuck <- stuck - 1
    } else {
      if (stuck == 5) {
        res[[length(res) + 1]] <- bestmat
        x1 <- largestincon(bestmat)
        x2 <- sample(mindex[-x1], 2)
        bestmat <- flip1(bestmat, c(x1[1], x2[1]))
        bestmat <- flip1(bestmat, c(x1[2], x2[2]))
        bestmetrics <- c(.incon(bestmat), .sincon(bestmat))
        stuck <- 0
      }
    }
    if (sum(c(.incon(bestmat), .sincon(bestmat))) == 0) {
      res <- list(bestmat)
      break
    }
  }

  # find the best matrix (or matrices with equal minI and minSI)
  mini <- unlist(lapply(res, .incon))
  mini <- which(mini == min(mini))
  res <- res[mini]
  minsi <- unlist(lapply(res, .sincon))
  minsi <- which(minsi == min(minsi))
  res <- res[minsi]
  res <- res[which(!duplicated(lapply(res, colnames)))]

  # put tied (incl 0/0) relationships back in
  for (i in seq_along(res)) {
    res[[i]] <- mat[rownames(res[[i]]), colnames(res[[i]])]
  }

  # check for adjancents with ties
  # and reorder those according to number of individuals dominated
  res <- lapply(res, laststep)
  res <- res[which(!duplicated(lapply(res, colnames)))]

  names(bestmetrics) <- c("I", "SI")

  res2 <- unique(lapply(res, colnames))
  if (length(res2) > 1) {
    if (printmessages) message("more than 1 solution")

    temp <- matrix(ncol = length(res), nrow = nrow(mat))
    rownames(temp) <- colnames(mat)
    for (i in seq_len(nrow(mat))) {
      temp[i, ] <- unlist(lapply(res, function(x) {
        which(colnames(x) == rownames(temp)[i])
      }
      ))
    }
  }

  if (printmessages) {
    cat("I =", .incon(res[[1]]), "\n")
    cat("SI =", .sincon(res[[1]]), "\n")
  }

  return(res)
}
