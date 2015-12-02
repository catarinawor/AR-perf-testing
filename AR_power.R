#' Perform a power analysis for AR-perf-testing to determine how many samples
#' are needed per year for age and length compositions to internally estimate
#' autocorrelation inside Stock Synthesis.

#' Ensure that cod was the last species simulated to ensure that
#' the recruitment deviations will be specific for cod
if (spp != "cod") {
  stop(paste("The last species ran was", spp, "and not cod.\n",
    "Please generate recruitment deviations specific for cod\n",
    "before running the rest of AR_steepness.R -KFJ"))
}


#' Setup
nyears.min <- 10
nyears.power <- seq(start + burnin, nyears - my.forecasts - nyears.min, by = 100)
data.power <- c(200, 500)
scenarios.power <- NULL
basenumber <- 900000


#' Write new case files
setwd(case_folder)
for (i in seq_along(nyears.power)) {
  scenarios.power <- c(scenarios.power,
    expand_scenarios(species = my.spp, cases = list(
    D = 90 + i,
    E = basenumber/1000 + my.forecasts,
    A = basenumber + 10000 * i + data.power,
    # L = c(0, basenumber + 10000 * i + data.power),
    L = c(basenumber + 10000 * i + data.power),
    F = 90 + i)))
#' Fishing history
  writeF(fvals =
    c(rep(0, nyears.power[i] - 1),  # burnin with no fishing
      rep(fmsy, nyears + start - nyears.power[i] - my.forecasts), # fishing at Fmsy
      rep(0, my.forecasts)), # forecast period with no fishing
    species = spp, case = 90 + i,
    comment = "#Power analysis\n#No fishing during forecast\n",
    start = start, end = nyears)
#' Composition data
  for (ii in seq_along(data.power)) {
  for (iii in c("agecomp", "lcomp")){
    case_comp(fleets = 2,
      Nsamp = list(rep(data.power[ii], length(nyears.power[i]:(nyears - my.forecasts)))),
      years = list(nyears.power[i]:(nyears - my.forecasts)),
      cpar = 1,
      type = iii,
      case = basenumber + 10000 * i + data.power[ii], spp = my.spp)
  }}
#' Index data
  case_index(fleets = 2,
    years = list(nyears.power[i]:(nyears - my.forecasts)),
    sd = list(my.dats[3]), case = 90 + i, spp = my.spp)
}
#' Fix fishery selectivity at the truth
writeLines(c("natM_type; 1Parm", "natM_n_breakpoints; NULL",
  "natM_lorenzen; NULL", "natM_val; c(NA, -1)",
  "par_name; c(\"SizeSel_1P_1_Fishery\", \"SizeSel_1P_3_Fishery\")",
  paste("par_int; c(50.8, 5.1)"),
  "par_phase; rep(-99, 2)", "forecast_num; 20"), paste0("E920-", my.spp, ".txt"))
#' Return to the main folder
setwd("..")


#' Run the simulation
  # for (ar in 1:length(AR)) {
  for (ar in 5) {
    run_ss3sim(
      iterations = 1:N,
      scenarios = scenarios.power,
      user_recdevs = EpsList[[ar]],
      bias_adjust = FALSE,
      om_dir = file.path(spp, "om"), em_dir = file.path(spp, "em"),
      hess_always = TRUE,
      parallel = doparallel, parallel_iterations = doparallel,
      case_files = my.cases, case_folder = case_folder,
      user_recdevs_warn = verbose
    )

    sppname <- paste0(substr(spp, 1, 1), letters[ar], "y")
    for (scen in grep(spp, scenarios.power, value = TRUE)) {
      # Copy to a new species name for the level of autocorrelation
      newscenario <- gsub(spp, sppname, scen)
      rename_folder(scen, newscenario, parallel = doparallel, numcores = numcores)
      # Copy the no bias to a new name before estimating the ramp
      system(paste("xcopy", newscenario, gsub("y$", "n", newscenario), "/E /S /H /I"),
         show.output.on.console = verbose)
      # Perform the bias ramp adjustment
      foreach(it = 1:N) %dopar% {
        fitbias(scenario = newscenario, iteration = it,
        dir = dir.main, numberofcolumns = ncols, verbose = verbose)
      }

    # # z == Set AR to zero; t == Set AR to true AR; x == externally estimate AR
    # for (type in "x") {
    #   # Copy folder with new species name given the type
    #   system(paste("xcopy", newscenario, gsub(spp, paste0(sppname, type), scen),
    #     "/E /S /H /I"), show.output.on.console = verbose)
    #   # Use parallel processing to loop through each iteration
    #   foreach(it = 1:N) %dopar% {
    #     em(it = it, type = type, truear = NULL, dir = dir.main,
    #     sppold = spp, sppnew = sppname, scenario = scen, verbose = verbose)
    #   } # End loop over each iteration
    # } # End loop over each type of EM
   } # End loop over each scenario
  } # End loop over AR level

get_results_all(overwrite_files = TRUE, parallel = doparallel)
sc <- read.csv("ss3sim_scalar.csv")
sc$bias <- ifelse(sc$max_bias_adj_em == -1, "no", "yes")
sc$A <- factor(as.character(sc$A), levels = levels(sc$A), labels = gsub("91", "-", levels(sc$A)))
sc$L <- factor(as.character(sc$L), levels = levels(sc$L), labels = gsub("91", "-", levels(sc$L)))

g <- ggplot(subset(sc, E == "E20")) +
theme_bw() +
  theme(plot.background = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 0, colour = "white")
  ) +
geom_boxplot(aes(x = bias, y = SR_autocorr_em)) +
facet_grid(A ~ L, scales = "fixed") +
geom_hline(yintercept = AR[5], col = "red")
ggsave(file.path(fig_folder, "power_rho.png"), g, height = 8, width = 8)
