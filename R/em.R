#' @param it The iteration folder for the given estimation method.
#' @param sppold A character value of the true species name.
#' @param sppnew A character value of the new species name.
#' @param scenario The true scenario name.
#' @param dir The base directory where all folders are housed.
#' @param type If \code{type == "z"} then autocorrelation is set to zero.
#'   If \code{type == "t"} autocorrelation is set to the true value as specified
#'   in \code{ar}.
#'   If \code{type == "x"} autocorrelation is set to an external estimate.
#' @param truear A \code{numeric} value of the true value of autocorrelation.
#' @param verbose A logical value whether or not information should be printed to
#'   the screen.

em <- function(it, sppold, sppnew, scenario, dir,
  type = c("z", "t", "x"), truear = NULL, verbose = FALSE) {

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  type <- match.arg(type, choices = c("z", "t", "x"), several.ok = FALSE)
  # For template LO HI INIT ExpectedVal Type SD PHASE
  # ExpectedVal is ignored if Type == -1 or 1
  # uniform prior
  artemplate <- "-0.99 0.99 0 0 -1 0 -5 # SR_autocorr"
  currentdirectory <- gsub(sppold, paste0(sppnew, type), scenario)
  print(currentdirectory)
  flush.console()
  empath <- file.path(dir, currentdirectory, it, "em")
  setwd(empath)

  # Perform a check to see if Report.sso exists, if not rerun SS3.
  # If there is still no report file, then move out of the function.
  if (!file.exists("Report.sso")) {
    bin <- paste(ss3sim::get_bin(), "exe", sep = ".")
    file.copy(bin, file.path(empath, basename(bin)))
    system(basename(bin), invisible = TRUE, show.output.on.console = verbose)
    if (!file.exists("Report.sso")) {
      keepgoodfiles()
      message <- paste(empath, "did not produce a report file.")
      return(message)
    }
  }

  emctl <- readLines("em.ctl")
  changeline <- grep("# SR_autocorr", emctl)
  arval <- strsplit(emctl[changeline], "[[:space:]]+")[[1]]
  arval <- arval[arval != ""][3]

  if (type == "z") emctl[changeline] <- artemplate
  if (type == "t") {
    temp <- artemplate
    temp[3] <- truear
    emctl[changeline] <- temp
  }
  if (type == "x") {
    results <- r4ss::SS_output(dir = getwd(), ncols = ncols,
      covar = file.exists("CompReport.sso"),
      forecast = FALSE,
      NoCompOK = !file.exists("CompReport.sso"),
      warn = verbose, verbose = verbose, printstats = verbose, hidewarn = !verbose)
    recruits <- results$timeseries$Recruit_0
    r0 <- results$parameters[results$parameters$Label == "SR_LN(R0)", "Value"]
    recdev <- results$parameters[
      grep("Main_RecrDev_", results$parameters$Label), "Value"]
    # Calculate the autocorrelation
    forss <- stats::acf(recdev[timeframe[1]:timeframe[2]], plot = FALSE,
      na.action = na.omit)[lag]$acf[1]
    temp <- strsplit(artemplate, "[[:space:]]+")[[1]]
    temp[3] <- round(forss, 5)
    emctl[changeline] <- paste(temp, collapse = " ")
  } # End if type is external

  writeLines(emctl, "em.ctl")

  # Delete all files produced by a run and keep just those needed for this run
  # If the run fails to converge this will ensure a leftover .covar file is not present
  keepgoodfiles()
  bin <- paste(ss3sim::get_bin(), "exe", sep = ".")
  file.copy(bin, file.path(empath, basename(bin)))
  system(basename(bin), invisible = TRUE, show.output.on.console = verbose)

  setwd(dir)
  fitbias(scenario = currentdirectory, iteration = it, dir = dir, verbose = verbose)

}
