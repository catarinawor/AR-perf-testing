#' Calculate confidence intervals
calulate_ci <- function(x, coverage = 0.5, verbose = FALSE) {
  zscore <- qnorm(coverage / 2, lower.tail = FALSE)
  if (verbose) {
    message(paste("A coverage of", coverage, "led to a zscore of",
      round(zscore, 3)))
  }
  keep <- x[x$year %in% 81:100, ]
  gnames <- grep("Value\\.", colnames(keep), value = TRUE)
  gnames <- grep("_em$", gnames, value = TRUE)
  gnames <- sapply(strsplit(gnames, "\\."), "[", 2)
  for(i in seq_along(gnames)) {
    if(grepl("OFLCatch", gnames[i])) next
    cols <- grep(gnames[i], colnames(keep))
    keep[, NCOL(keep) + 1] <- keep[, cols[1]] + zscore * keep[, cols[2]]
    colnames(keep)[NCOL(keep)] <- paste0("high.", gnames[i])
    keep[, NCOL(keep) + 1] <- keep[, cols[1]] - zscore * keep[, cols[2]]
    colnames(keep)[NCOL(keep)] <- paste0("low.", gnames[i])
    keep[, NCOL(keep) + 1] <- apply(keep, 1, function(x) {
      gb <- as.numeric(x[paste0("high.", gnames[i])]) >
            as.numeric(x[gsub("_em", "_om", paste0("Value.", gnames[i]))]) &&
            as.numeric(x[paste0("low.", gnames[i])]) <
            as.numeric(x[gsub("_em", "_om", paste0("Value.", gnames[i]))])
      return(gb)
      })
    colnames(keep)[NCOL(keep)] <- paste0("in.", gnames[i])
  }
  invisible(keep)
}
