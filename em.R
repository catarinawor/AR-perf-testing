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

  setwd(dir)
  on.exit(setwd(dir))

  type <- match.arg(type, choices = c("z", "t", "x"), several.ok = FALSE)

  currentdirectory <- gsub(sppold, paste0(sppnew, type), scenario)
  print(currentdirectory)
  flush.console()
  scenarioname <- gsub(sppold, sppnew, scenario)

  newpath <- file.path(dir, currentdirectory, it, "em")
  if (!file.exists(newpath)) {
    stop(paste(newpath, "does not exist you are in", getwd()))
  }
  setwd(newpath)
  emctl <- readLines("em.ctl")
  changeline <- grep("# SR_autocorr", emctl)

  if (type == "z") emctl[changeline] <- "-1 1 0 0 -1 0 -5 # SR_autocorr"
  if (type == "t") emctl[changeline] <- paste("-1 1", truear, "0 -1 0 -5 # SR_autocorr")
  if (type == "x") {
    # Check for convergence
    grad <- readLines("Report.sso", n = 18)
    grad <- grep("Convergence_Level:", grad, value = TRUE)
    grad <- as.numeric(strsplit(grad, "[[:space:]]")[[1]][2])
    if (!grad < 0.01) {
      unlink(it, recursive = TRUE)
      return()
    } # End if for non-convergence
    #' Check to see if certain files exist for obtaining the results
    checkexists <- mapply(file.exists, checkfiles)
    results <- r4ss::SS_output(dir = getwd(), ncols = ncols,
      covar = checkexists["CompReport.sso"],
      forecast = FALSE,
      NoCompOK = !checkexists["CompReport.sso"],
      warn = verbose, verbose = verbose, printstats = verbose, hidewarn = !verbose)
    recruits <- results$timeseries$Recruit_0
    r0 <- results$parameters[results$parameters$Label == "SR_LN(R0)", "Value"]
    recdev <- results$parameters[
      grep("Main_RecrDev_", results$parameters$Label), "Value"]
    # Calculate the autocorrelation
    forss <- stats::acf(recdev[timeframe[1]:timeframe[2]], plot = FALSE,
      na.action = na.omit)[lag]$acf[1]
    emctl[changeline] <- paste("-1 1", forss, "0 -1 0 -5 # SR_autocorr")
  } # End if type is external

  writeLines(emctl, "em.ctl")

  # Delete all files produced by a run and keep just those needed for this run
  # If the run fails to converge this will ensure a leftover .covar file is not present
  ignore <- unlink(list.files()[!list.files() %in% c("em.ctl", "ss3.dat", "forecast.ss", "starter.ss")])
  ignore <- system(ss3sim::get_bin(), show.output.on.console = verbose)

  return(print(newpath))
}
