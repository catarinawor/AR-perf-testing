rename_folder <- function(old, new, parallel = TRUE, numcores) {
  if (!file.exists(old)) stop(paste("The dir", old, "does not exist"))
  check <- file.rename(old, new)
  if (!check) { # An if statement in case a file was left open
    message("The folder", old, "was not copied properly, trying again.")
    closeAllConnections()
    if (parallel) {
      cl <- makeCluster(numberofcores)
      registerDoParallel(cl)
    }
    check <- file.rename(old, new)
    if (!check) browser()
  }
  invisible(check)
}
