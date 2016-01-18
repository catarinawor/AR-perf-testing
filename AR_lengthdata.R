#' Using only age data, determine how many years of data are needed to
#' estimate rho compared to a full time series (i.e., 25:80).


#' Ensure that cod was the last species simulated to ensure that
#' the recruitment deviations will be specific for cod
if (spp != "cod") {
  stop(paste("The last species ran was", spp, "and not cod.\n",
    "Please generate recruitment deviations specific for cod\n",
    "before running the rest of AR_lengthdata.R -KFJ"))
}


#' Setup
nyears.lengthdata <- seq(start + burnin, nyears - my.forecasts - 9, length.out = 4)
nages <- 100
scenarios.lengthdata <- NULL


#' Write new case files
setwd(case_folder)
for (i in seq_along(nyears.lengthdata)) {
  scenarios.lengthdata <- c(scenarios.lengthdata,
    expand_scenarios(species = my.spp, cases = list(
    D = nages + i,
    E = nages + my.forecasts,
    A = nages + i,
    L = 0,
    F = 1)))
#' Composition data
  case_comp(fleets = c(1, 2),
    Nsamp = list(rep(nages, length(nyears.lengthdata[i]:(nyears - my.forecasts))),
                 rep(nages, length(nyears.lengthdata[i]:(nyears - my.forecasts)))),
    years = list(nyears.lengthdata[i]:(nyears - my.forecasts),
                 nyears.lengthdata[i]:(nyears - my.forecasts)),
    cpar = rep(1, 2),
    type = "agecomp",
    case = nages + i, spp = my.spp)
#' Index data
  case_index(fleets = 2,
    years = list(nyears.lengthdata[i]:(nyears - my.forecasts)),
    sd = list(my.dats[3]), case = nages + i, spp = my.spp)
}
#' Fix growth at the truth
writeLines(c("natM_type; 1Parm", "natM_n_breakpoints; NULL",
  "natM_lorenzen; NULL", "natM_val; c(NA, -1)",
  "par_name; c(\"L_at_Amin_Fem_GP_1\", \"L_at_Amax_Fem_GP_1\", \"VonBert_K_Fem_GP_1\", \"CV_young_Fem_GP_1\", \"CV_old_Fem_GP_1\")",
  paste("par_int; rep(NA, 5)"),
  "par_phase; rep(-99, 5)", "forecast_num; 20"), paste0("E", nages + my.forecasts, "-", my.spp, ".txt"))
#' Return to the main folder
setwd("..")


#' Run the simulation
for (ar in 1:length(AR)) {
    run_ss3sim(
      iterations = 1:N,
      scenarios = scenarios.lengthdata,
      user_recdevs = EpsList[[ar]],
      bias_adjust = FALSE,
      om_dir = file.path(spp, "om"), em_dir = file.path(spp, "em"),
      hess_always = TRUE,
      parallel = doparallel, parallel_iterations = doparallel,
      case_files = my.cases, case_folder = case_folder,
      user_recdevs_warn = verbose
    )

    sppname <- paste0(substr(spp, 1, 1), letters[ar], "y")
    for (scen in grep(spp, scenarios.lengthdata, value = TRUE)) {
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
      } # End loop over iteration of bias adjustment

      # # z == Set AR to zero; t == Set AR to true AR; x == externally estimate AR
      for (type in types) {
      # Copy folder with new species name given the type
      system(paste("xcopy", newscenario, gsub(spp, paste0(sppname, type), scen),
        "/E /S /H /I"), show.output.on.console = verbose)
      if (type == "t") {
        truear <- AR[ar]
      } else {
        truear <- NULL
      }
      # Use parallel processing to loop through each iteration
      foreach(it = 1:N) %dopar% {
        em(it = it, type = type, truear = truear, dir = dir.main,
        sppold = spp, sppnew = sppname, scenario = scen, verbose = verbose)
      } # End loop over each iteration of alternative AR estimation
      if (type == "z") {
        system(paste("xcopy", gsub(spp, paste0(sppname, "z"), scen),
          gsub(spp, paste0(sppname, "zx"), scen), "/E /S /H /I"),
          show.output.on.console = verbose)
        foreach(it = 1:N) %dopar% {
          em(it = it, type = "x", truear = truear, dir = dir.main,
          sppold = paste0(sppname, "z"), sppnew = paste0(sppname, "z"),
          scenario = gsub(spp, paste0(sppname, "z"), scen),
          verbose = verbose)
        }
      }
     } # End loop over each type of EM
   } # End loop over each scenario
  } # End loop over AR level
