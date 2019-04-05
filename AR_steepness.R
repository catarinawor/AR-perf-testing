#' Simulate steepness fixed at 1.00

#' Check cod was the last species ran, such that the recruitment
#' deviations will be specific for cod
if (spp != "cod") {
  stop(paste("The last species ran was", spp, "and not cod.\n",
    "Please generate recruitment deviations specific for cod\n",
    "before running the rest of AR_steepness.R -KFJ"))
}

#' Inputs needed to run the steepness exploratory runs.
newspp <- "ste"
arvalue <- tail(AR, 1)
dir.create(newspp)
if (file.exists(newspp)) unlink(newspp, recursive = TRUE)
ignore <- system(paste("xcopy", my.spp, newspp, "/E /S /H /I"),
  show.output.on.console = FALSE)

#' Change steepness in the OM to 1.0
r4ss::SS_changepars(dir = file.path(newspp, "om"),
  ctlfile = list.files(file.path(newspp, "om"), pattern = "\\.ctl"),
  newctlfile = list.files(file.path(newspp, "om"), pattern = "\\.ctl"),
  strings = "SR_BH_steep", newvals = 1.0, estimate = NULL)
steom <- readLines(list.files(file.path(newspp, "om"), pattern = "\\.par",
  full.names = TRUE))
steom[grep("# SR_parm\\[2\\]:", steom) + 1] <- 1.0
writeLines(steom, list.files(file.path(newspp, "om"), pattern = "\\.par",
  full.names = TRUE))
#' Change steepness in the EM to 1.0 & AR to the true value
r4ss::SS_changepars(dir = file.path(newspp, "em"),
  ctlfile = list.files(file.path(newspp, "em"), pattern = "\\.ctl"),
  newctlfile = list.files(file.path(newspp, "em"), pattern = "\\.ctl"),
  strings = c("SR_BH_steep", "SR_autocorr"), newvals = c(1.0, arvalue), estimate = FALSE)

#' Copy all case files
copyfiles <- list.files(case_folder, full.names = TRUE)
copyfiles <- copyfiles[!grepl(newspp, copyfiles)]
ignore <- file.copy(copyfiles, gsub(my.spp, newspp, copyfiles), overwrite = TRUE)

#' Calculate the new Fmsy and write a case file
dir.create(file.path(newspp, "fmsy"))
fmsy <- profile_fmsy(om_in = file.path(getwd(), newspp, "om"),
  file.path(newspp, "fmsy"), start = 0.05, end = 0.3)
fs <- ss3sim:::get_args(
  file.path(case_folder, paste0("F1-", my.spp[1], ".txt")))$fvals
fs[which(fs != 0)] <- fmsy[which.max(fmsy$eqCatch), "fValues"]

sink(file.path(case_folder, paste0("F1-", newspp, ".txt")))
writeLines("# Casefile for steepness == 1.0 of cod")
cat("years; c(", paste(1:length(fs), collapse = ", "), ")\n",
  "years_alter; c(", paste(1:length(fs), collapse = ", "), ")\n",
  "fvals; c(", paste(fs, collapse = ", "), ")\n", sep = "")
sink()

#' Run the simulation
  run_ss3sim(
    iterations = 1:N,
    scenarios = expand_scenarios(cases = list(D = 0, E = my.forecasts,
      A = my.dats[1], L = my.dats[1], F = 1), species = newspp),
    user_recdevs = EpsList[[length(AR)]],
    bias_nsim = 0,
    bias_adjust = FALSE,
    om_dir = file.path(newspp, "om"), em_dir = file.path(newspp, "em"),
    hess_always = TRUE, parallel = doparallel, parallel_iterations = doparallel,
    case_files = my.cases, case_folder = case_folder,
    user_recdevs_warn = verbose, show.output.on.console = verbose
  )

get_results_all(user_scenario = dir(pattern = newspp))
sc1 <- read.csv("ss3sim_scalar.csv")
ts1 <- read.csv("ss3sim_ts.csv")
ts1 <- calculate_re(ts1, add = TRUE)
plot_ts_boxplot(ts1, y = "SpawnBio_re", relative.error = TRUE)
ggplot2::ggsave(filename = "steepnessSSB.png",
  path = tail(dir(pattern = "fig"), 1))
dev.off()
