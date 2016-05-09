fitbias <- function(scenario, iteration, dir, numberofcolumns = 300, verbose) {

  old_wd <- getwd()
  on.exit(setwd(old_wd))

  empath <- file.path(dir, scenario, iteration, "em")
  failed <- paste("Fail:", empath)
  if (!file.exists(file.path(empath, "covar.sso"))) return(failed)
  if (!file.exists(file.path(empath, "Report.sso"))) return(failed)

  report <- r4ss::SS_output(empath, ncols = numberofcolumns, verbose = verbose,
    forecast = FALSE, printstats = verbose)
  r4ss::SS_fitbiasramp(report, verbose = verbose, print = verbose, plot = verbose,
    oldctl = file.path(empath, "control.ss_new"),
    newctl = file.path(empath, "em.ctl"))

  keepgoodfiles(dir = empath)
  bin <- normalizePath(ss3sim::get_bin())
  file.copy(bin, file.path(empath, basename(bin)))
  setwd(empath)
  system(basename(bin), invisible = TRUE, show.output.on.console = verbose)
}
