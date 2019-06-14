#' heatmap
#'
#' @param formula formula for plot
#' @param data data set for plot (typically a data frame)
#' @param xbreaks numeric, the breakpoints for the horizontal axis
#' @param ybreaks numeric, the breakpoints for the vertical axis
#' @param addvals add the response values to the plot
#' @param addN add the sample size to the plot
#' @param digits numeric: if response variable is plotted, round to this many digits (default is 1)
#' @param ... other parameters passed on to plot() or text()
#'
#' @return a plot
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics rect
#' @importFrom stats aggregate
#' @export
#'
#' @examples
#' xdata <- expand.grid(a = seq(0, 1, 0.1), b = seq(10, 20, 1))
#' xdata$resp <- rnorm(nrow(xdata))
#' heatmapplot(resp ~ a + b, data = xdata)
#'
#' set.seed(123)
#' xdata <- expand.grid(k = seq(8, 200, length.out = 31), shape = seq(0, 1, length.out = 31))
#' idata <- randomsequence(10, 50, reversals = 0.3)
#' allids <- colnames(idata$pres)[2:ncol(idata$pres)]
#' winner <- as.character(idata$seqdat$winner)
#' loser <- as.character(idata$seqdat$loser)
#'
#' myranks <- 1:length(allids)
#' names(myranks) <- allids
#'
#' for(i in 1:nrow(xdata)) {
#'   kv <- rep(xdata$k[i], length(winner))
#'   sv <- createstartvalues(ranks = myranks, shape = xdata$shape[i])$res
#'   res <- fastelo(WINNER = winner, LOSER = loser, ALLIDS = allids, KVALS = kv, STARTVALUES = sv,
#'                  ROUND = FALSE)
#'   xdata$ll[i] <- likelo(res)
#' }
#'
#' heatmapplot(ll ~ k + shape, data = xdata)

heatmapplot <- function(formula, data, xbreaks = NULL, ybreaks = NULL,
                        addvals = FALSE, addN = FALSE, digits = 1, ...) {

  allvars <- all.vars(formula)
  pdat <- data[, allvars[1]]
  xdat <- data[, allvars[2]]
  ydat <- data[, allvars[3]]
  if (sum(allvars %in% colnames(data)) != 3) {
    stop ("not all variables found...", call. = FALSE)
  }

  LO <- ifelse(length(table(xdat)) < 100, length(table(xdat)), 11)
  if (is.null(xbreaks)) xbreaks <- seq(min(xdat) - 0.00000001, max(xdat), length.out = LO)
  xmp <- diff(xbreaks)[1]
  xbreaks1 <- c(xbreaks - xmp / 2, max(xbreaks) + xmp / 2)

  LO <- ifelse(length(table(ydat)) < 100, length(table(ydat)), 11)
  if (is.null(ybreaks)) ybreaks <- seq(min(ydat) - 0.00000001, max(ydat), length.out = LO)
  ymp <- diff(ybreaks)[1]
  ybreaks1 <- c(ybreaks - ymp / 2, max(ybreaks) + ymp / 2)

  xcats <- cut(xdat, breaks = xbreaks1)
  ycats <- cut(ydat, breaks = ybreaks1)

  pdata <- data.frame(table(xcats, ycats))

  # aggregate (even if there is only one value per combination)
  pdata <- aggregate(pdat, by = list(xcats = xcats, ycats = ycats), mean)
  pdata$XN <- aggregate(pdat, by = list(xcats = xcats, ycats = ycats), length)$x

  pdata$xcoor <- xbreaks[as.numeric(pdata$xcats)]
  pdata$ycoor <- ybreaks[as.numeric(pdata$ycats)]

  # handle color gradient
  xcols <- colorRampPalette(colors = c("blue", "red"))
  colbreaks <- seq(min(pdata$x) - 0.00000001, max(pdata$x), length.out = 101)
  cmp <- diff(colbreaks)[1]
  colbreaks1 <- c(colbreaks - cmp / 2, max(colbreaks) + cmp / 2)
  pdata$colcats <- cut(pdata$x, breaks = colbreaks1)
  pdata$xcol <- xcols(length(colbreaks))[as.numeric(pdata$colcats)]

  # layout and legend
  layout(mat = matrix(2:1, ncol = 2), widths = c(4, 1))
  omars <- par("mar")
  par(mar = c(omars[1], 0, omars[3], 3))
  plot(0, 0, type = "n", ann = FALSE, xaxs = "i", axes = FALSE,
       ylim = range(colbreaks1), xlim = c(-0.75, 0.5))
  rect(xleft = -0.5, ybottom = seq(min(colbreaks1), max(colbreaks1), length.out = 101) - diff(colbreaks1)[1] / 2,
       xright = 0.5, ytop = seq(min(colbreaks1), max(colbreaks1), length.out = 101) + diff(colbreaks1)[1] / 2 + diff(colbreaks1)[1]*1.01,
       col = xcols(length(colbreaks)), border = NA)
  axis(4, lwd.ticks = 1, lwd = 0, las = 1, cex.axis = 0.8)

  par(mar = omars)
  # ellipsis stuff for plot and rectangles
  supplied <- list(...)
  defaults <- list(xlab = allvars[2], ylab = allvars[3], type = "n",
                xlim = range(xbreaks1), ylim = range(ybreaks1), x = 0, y = 0,
                bty = "n", xaxs = "i", yaxs = "i", axes = FALSE)
  defaults[names(supplied)] <- supplied
  do.call(plot, defaults)
  axis(1, lwd.ticks = 1, lwd = 0)
  axis(2, lwd.ticks = 1, lwd = 0, las = 1)

  rect(xleft = pdata$xcoor - xmp / 2, ybottom = pdata$ycoor - ymp / 2,
            xright = pdata$xcoor + xmp / 2 + xmp * 1.01, ytop = pdata$ycoor + ymp / 2 + ymp * 1.01,
            col = pdata$xcol, border = NA)

  defaults <- list(col = "white", x = pdata$xcoor, y = pdata$ycoor,
                   labels = round(pdata$x, digits = digits))
  defaults[names(supplied)] <- supplied
  if (addvals) do.call(text, defaults)
  defaults <- list(col = "white", x = pdata$xcoor, y = pdata$ycoor,
                   labels = pdata$XN)
  defaults[names(supplied)] <- supplied
  if (addN) do.call(text, defaults)

  # reset layout and margins
  invisible(layout(1))
  par(mar = omars)
}
