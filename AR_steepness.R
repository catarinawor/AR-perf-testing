#' Simulate steepness fixed at 1.00

newspp <- "ste"
arvalue <- tail(AR, 1)
dir.create(newspp)
ignore <- system(paste("xcopy", my.spp, newspp, "/E /S /H /I"))

#' Change steepness in the OM to 1.0
r4ss::SS_changepars(dir = file.path(newspp, "om"),
  ctlfile = list.files(file.path(newspp, "om"), pattern = "\\.ctl"),
  newctlfile = list.files(file.path(newspp, "om"), pattern = "\\.ctl"),
  strings = "SR_BH_steep", newvals = 1.0, estimate = NULL)
#' Change steepness in the EM to 1.0
r4ss::SS_changepars(dir = file.path(newspp, "em"),
  ctlfile = list.files(file.path(newspp, "em"), pattern = "\\.ctl"),
  newctlfile = list.files(file.path(newspp, "em"), pattern = "\\.ctl"),
  strings = "SR_BH_steep", newvals = 1.0, estimate = NULL)
#' Change AR to the true value in the EM to 1.0
r4ss::SS_changepars(dir = file.path(newspp, "em"),
  ctlfile = list.files(file.path(newspp, "em"), pattern = "\\.ctl"),
  newctlfile = list.files(file.path(newspp, "em"), pattern = "\\.ctl"),
  strings = "SR_autocorr", newvals = arvalue, estimate = FALSE)

#' Copy all case files
copyfiles <- list.files(case_folder, full.names = TRUE)
ignore <- file.copy(copyfiles, gsub(my.spp, newspp, copyfiles))

#' Calculate the new Fmsy and write a case file
dir.create(file.path(newspp, "fmsy"))
fmsy <- profile_fmsy(om_in = file.path(getwd(), newspp, "om"),
  file.path(newspp, "fmsy"), end = 0.75)
fs <- ss3sim:::get_args(file.path(case_folder, paste0("F1-", my.spp, ".txt")))$fvals
fs[which(fs != 0)] <- fmsy[which.max(fmsy$eqCatch), "fValues"]

sink(file.path(case_folder, paste0("F1-", newspp, ".txt")))
writeLines("# Casefile for steepness == 1.0 of cod")
cat("years; c(", paste(0:length(fs), collapse = ", "), ")\n",
  "years_alter; c(", paste(0:length(fs), collapse = ", "), ")\n",
  "fvals; c(", paste(fs, collapse = ", "), ")\n", sep = "")
sink()

#' Run the simulation
  run_ss3sim(
    iterations = 1:N,
    scenarios = expand_scenarios(cases = list(D = 0, E = my.forecasts,
      A = my.dats[1], L = my.dats[1], F = 1), species = newspp),
    user_recdevs = EpsList[[length(AR)]],
    bias_nsim = NB,
    bias_adjust = TRUE,
    om_dir = file.path(spp, "om"), em_dir = file.path(spp, "em"),
    hess_always = TRUE, parallel = doparallel, parallel_iterations = doparallel,
    case_files = my.cases, case_folder = case_folder,
    user_recdevs_warn = verbose, show.output.on.console = verbose
  )
