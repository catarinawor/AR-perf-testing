keepgoodfiles <- function(files = c("em.ctl", "control.ss_new", "ss3.dat", "forecast.ss", "starter.ss"),
  dir = getwd()) {

  if (!file.exists(dir)) stop(paste("The directory", dir, "does not exist"))

  allfiles <- list.files(dir, full.names = TRUE)
  ignore <- unlink(allfiles[-grep(paste(files, collapse = "|"), allfiles)])

}
