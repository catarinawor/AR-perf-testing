#' Simulate more realistic data to determine
#' if the results (i.e., that it is better to externally estimate AR)
#' hold in the face of data and an EM that looks more like what a
#' stock assessment scientist would see in actuality.

#' The scenario can be summarized as follows:
#' sigma_r = 0.6
#' survey CV = 0.25
#' Estimate steepness with a beta prior
#' Estimate survey and fishery logistic selectivity
#' Fix growth at the true value
#' Use 100 multinomial fishery and survey age samples / year
#' Use all years of data

#' Specify the species name for the messy data
spp <- "mes"
if (file.exists(spp)) unlink(spp, recursive = TRUE)
dir.create(spp, showWarnings = verbose)
system(paste("xcopy", my.spp, spp, "/E /S /H /I"),
  show.output.on.console = verbose)

#' Chose the value of 0.6 because it represents the upper limit of sigma_r
#' values tested in the factorial design with respect to sigma_r.
#' At this higher value with everything else correctly specified the model
#' was still able to estimate auto-correlated recruitment levels, so by
#' staying within the bounds of the upper limit explored we know that it is
#' due to the messiness of the data or the EM and not that it cannot estimate
#' it.
mes_sigmar <- 0.6

#' Create a less informative index of abundance
#' 0.25 was suggested by EB
mes_survcv <- 0.25

#' Use an AR value of 0.5
mes_AR <- 0.5

#' Create a new OM for "mes"
#' Fix par files
  ompar <- file.path(spp, "om", "ss3.par")
  par <- readLines(ompar)
  par[grep("SR_parm\\[3\\]", par) + 1] <- mes_sigmar
  writeLines(par, ompar)
#' Fix om control files
  r4ss::SS_changepars(dir = file.path(spp, "om"),
    ctlfile = "ss3.ctl",
    newctlfile = "ss3.ctl",
    strings = "SR_sigmaR",
    newvals = mes_sigmar,
    estimate = FALSE,
    verbose = verbose,
    newphs = NULL)

#' Generate new recruitment deviations for the new level of sigma_r
SDcond <- mes_sigmar * sqrt(1 - mes_AR^2)
mes_recdevs <- matrix(0, nrow = nyears, ncol = N + 5)
  # Create a temporary matrix with 1 column
  Eps_s <- matrix(0, nrow = NROW(mes_recdevs), ncol = 1)
  for (i in 1:NCOL(mes_recdevs)) {
    set.seed(i)
    # Bias correction added
    Eps_k <- rnorm(NROW(mes_recdevs), mean = 0, sd = SDcond)
    Eps_s[1] <- Eps_k[1] * mes_sigmar / SDcond
    for (t in 2:NROW(mes_recdevs)) {
      Eps_s[t] <- Eps_s[t - 1] * mes_AR + Eps_k[t]
    }
    mes_recdevs[, i] <- Eps_s - mes_sigmar^2 / 2
  }
if (verbose) {
  message("exponentiated mean equals ", round(mean(exp(mes_recdevs)), 3))
  message("marginal standard deviation equals ",
    round(sd(mes_recdevs), 3))
  message("autocorrelation equals ", round(mean(apply(mes_recdevs, MARGIN = 2,
    FUN = function(vec){var(vec[-length(vec)], vec[-1]) / var(vec)})), 3))
}

#' Create a new EM for "mes"
#' Set sigma_r to its true value of mes_sigmar
  r4ss::SS_changepars(dir = file.path(spp, "em"),
    ctlfile = "ss3.ctl",
    newctlfile = "ss3.ctl",
    strings = "SR_sigmaR",
    newvals = mes_sigmar,
    estimate = FALSE,
    verbose = verbose,
    newphs = NULL)
#' Turn on the estimation of steepness and place a prior on its estimation
#' 0.2 1 0.65 0.7 -1 0.05 -4 == SR_BH_steep in the simulation EM
#' here I am using a beta prior based on that found in the sablefish and
#' widow rockfish stock assessments
  emctl <- file.path(spp, "em", "ss3.ctl")
  ctl <- readLines(emctl)
  ctl[grep("SR_BH_steep", ctl)] <- "0.2 1 0.65 0.65 2 0.147 4 # SR_BH_steep"
  writeLines(ctl, emctl)
#' Turn on selectivity parameters
  r4ss::SS_changepars(dir = file.path(spp, "em"),
    ctlfile = "ss3.ctl",
    newctlfile = "ss3.ctl",
    strings = c("SizeSel_1P_1_Fishery", "SizeSel_1P_3_Fishery",
                "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey"),
    newvals = NULL,
    estimate = TRUE,
    verbose = verbose,
    newphs = c(2, 3, 2, 3))

#' Create new case files
#' F
  # Do not calculate a new Fmsy because profile_fmsy goes into the OM and changes
  # the SR_Parm[3] to 0.001, and the only thing that is different in our OM from
  # the original OM is that SR_Parm[3] is increased.
  # Using this version of SS3 led to a different Fmsy when I was playing around
  # with this, but I decided to keep the old Fmsy of 0.1210667 instead of changing
  # to the newly determined value of 0.11596 because all of the other scenarios
  # used the former value and I do not expect the results to change.
  ignore <- file.copy(file.path(case_folder, paste0("F0-", my.spp[1], ".txt")),
    file.path(case_folder, paste0("F0-", spp, ".txt")),
    overwrite = TRUE)
  index <- 3
#' D
#' Generate a case file for the survey with a cv of 0.25
  currentdir <- getwd()
  setwd(case_folder)
  case_index(fleets = 2,
    years = list(nyears.lengthdata[index]:(nyears - my.forecasts)),
    sd = list(mes_survcv), case = index, spp = spp)
  setwd(currentdir)
#' E
#' Copy the case file that turns off growth
  ignore <- file.copy(
    file.path(case_folder, paste0("E1", my.forecasts, "-", my.spp, ".txt")),
    file.path(case_folder, paste0("E1", my.forecasts, "-", spp, ".txt")),
    overwrite = TRUE)
#' A
  ignore <- file.copy(
    file.path(case_folder, paste0("agecomp", my.dats[1] + index, "-", my.spp, ".txt")),
    file.path(case_folder, paste0("agecomp", my.dats[1] + index, "-", spp, ".txt")),
    overwrite = TRUE)
#' L
  ignore <- file.copy(
    file.path(case_folder, paste0("lcomp", 0, "-", my.spp, ".txt")),
    file.path(case_folder, paste0("lcomp", 0, "-", spp, ".txt")),
    overwrite = TRUE)

#' Run the simulation
  run_ss3sim(
    iterations = 1:N,
    scenarios = expand_scenarios(
      cases = list(D = index, E = paste0("1", my.forecasts),
      A = my.dats[1] + index, L = 0, F = 0), species = spp),
    user_recdevs = mes_recdevs,
    bias_nsim = FALSE,
    om_dir = file.path(spp, "om"), em_dir = file.path(spp, "em"),
    hess_always = TRUE, parallel = doparallel, parallel_iterations = doparallel,
    case_files = my.cases, case_folder = case_folder,
    user_recdevs_warn = verbose, show.output.on.console = verbose
  )

    sppname <- paste0(substr(spp, 1, 1), letters[which(AR == mes_AR)], "y")
    # Find the scenario name for messy data
    scen <- dir(pattern = paste0("-", spp))
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
          truear <- mes_AR
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
