factAR <- function(x, levels = c(-0.25, 0, 0.25, 0.5, 0.75, 0.9)) {
  levels <- format(levels, nsmall = 2)
  beg <- gsub("^c|^sr[[:alpha:]]", "", x)
  beg <- substring(beg, 1, 1)
  answer <- levels[match(beg, letters)]
  return(answer)
}

factEM <- function(x,
  levels = c("True", "Zero", "Internal", "int_ext", "External")) {
  truelevels <- levels(x)
  x <- as.character(x)
  x[x %in% grep("y$|n$", truelevels, value = TRUE)] <- levels[3]
  x[x %in% grep("t$", truelevels, value = TRUE)] <- levels[1]
  x[x %in% grep("yx$|nx$", truelevels, value = TRUE)] <- levels[4]
  x[x %in% grep("z$", truelevels, value = TRUE)] <- levels[2]
  x[x %in% grep("zx$", truelevels, value = TRUE)] <- levels[5]
  x <- factor(x, levels = levels, labels = levels)
  return(x)
}
