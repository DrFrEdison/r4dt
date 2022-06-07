matplotp <- function (x, y, type = "l", lty = 1, lwd = 1, lend = par("lend"), 
                      pch = NULL, col = 1:6, cex = NULL, bg = NA, xlab = expression(paste(lambda,  " in nm")), 
                      ylab = "AU", xlim = NULL, ylim = NULL, log = "", ..., 
                      add = FALSE, verbose = getOption("verbose")) 
{
  paste.ch <- function(chv) paste0("\"", chv, "\"", 
                                   collapse = " ")
  str2vec <- function(string) {
    if (nchar(string, type = "c")[1L] > 1L) 
      strsplit(string[1L], NULL)[[1L]]
    else string
  }
  xlabel <- if (!missing(x)) 
    deparse1(substitute(x))
  ylabel <- if (!missing(y)) 
    deparse1(substitute(y))
  if (missing(x)) {
    if (missing(y)) 
      stop("must specify at least one of 'x' and 'y'")
    else x <- seq_len(NROW(y))
  }
  else if (missing(y)) {
    y <- x
    ylabel <- xlabel
    x <- seq_len(NROW(y))
    xlabel <- ""
  }
  if (is.matrix(x)) {
    n <- nrow(x)
  }
  else if (!is.null(dim(x))) {
    n <- nrow(x <- as.matrix(x))
  }
  else {
    n <- length(x)
    dim(x) <- c(n, 1L)
  }
  if (is.matrix(y)) {
  }
  else if (!is.null(dim(y))) {
    y <- as.matrix(y)
  }
  else {
    dim(y) <- c(length(y), 1L)
  }
  if (n != nrow(y)) 
    stop("'x' and 'y' must have same number of rows")
  kx <- ncol(x)
  ky <- ncol(y)
  if (!kx || !ky) 
    return(invisible())
  if (FALSE) 
    if (kx > 1L && ky > 1L && kx != ky) 
      stop("'x' and 'y' must have only 1 or the same number of columns")
  k <- max(kx, ky)
  type <- str2vec(type)
  if (is.null(pch)) {
    pch <- c(1L:9L, 0L, letters, LETTERS)
    if (k > length(pch) && any(type %in% c("p", "o", 
                                           "b"))) 
      warning("default 'pch' is smaller than number of columns and hence recycled")
  }
  else if (is.character(pch)) 
    pch <- str2vec(pch)
  if (verbose) 
    message("matplot: doing ", k, " plots with ", 
            paste0(" col= (", paste.ch(col), ")"), 
            paste0(" pch= (", paste.ch(pch), ")"), 
            " ...\n", domain = NA)
  xy <- xy.coords(x, y, xlabel, ylabel, log = log, recycle = TRUE)
  if (is.null(xlab)) 
    xlab <- xy$xlab
  if (is.null(ylab)) 
    ylab <- xy$ylab
  if (is.null(xlim)) 
    xlim <- range(xy$x[is.finite(xy$x)])
  if (is.null(ylim)) 
    ylim <- range(xy$y[is.finite(xy$y)])
  if (length(type) < k) 
    type <- rep_len(type, k)
  if (length(lty) < k) 
    lty <- rep_len(lty, k)
  if (length(lend) < k) 
    lend <- rep_len(lend, k)
  if (length(lwd) < k && !is.null(lwd)) 
    lwd <- rep_len(lwd, k)
  if (length(pch) < k) 
    pch <- rep_len(pch, k)
  if (length(col) < k) 
    col <- rep_len(col, k)
  if (length(bg) < k) 
    bg <- rep_len(bg, k)
  if (is.null(cex)) 
    cex <- 1
  if (length(cex) < k) 
    cex <- rep_len(cex, k)
  ii <- seq_len(k)
  dev.hold()
  on.exit(dev.flush())
  if (!add) {
    ii <- ii[-1L]
    plot(x[, 1L], y[, 1L], type = type[1L], xlab = xlab, 
         ylab = ylab, xlim = xlim, ylim = ylim, lty = lty[1L], 
         lwd = lwd[1L], lend = lend[1L], pch = pch[1L], col = col[1L], 
         cex = cex[1L], bg = bg[1L], log = log, ...)
  }
  for (i in ii) lines(x[, 1L + (i - 1L)%%kx], y[, 1L + (i - 
                                                          1L)%%ky], type = type[i], lty = lty[i], lwd = lwd[i], 
                      lend = lend[i], pch = pch[i], col = col[i], cex = cex[i], 
                      bg = bg[i])
  invisible()
}