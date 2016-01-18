# Generate recruitment deviations specific for each
# level of sigma_r that you wish to include

# SDmarg is determined in an earlier script by
# looking for the sigmar in the om control file
# Here we want to create a vector of plausible
# levels of sigmar
my.sigmar <- seq(SDmarg * 0.5, SDmarg * 1.5, length.out = 5)
sigmarspp <- paste0("sr", letters[seq_along(my.sigmar)])

# Generate recruitment devtions for each sigmar
sigmarecdev <- list()
 for (sigmar in my.sigmar) {
   # Create recruitment deviations
   # A list is generated with one matrix per level of AR
   tempcond <- sigmar * sqrt(1 - AR^2)
   temp <- list()
   for (ar in seq_along(AR)) {
     temp[[ar]] <- matrix(0, nrow = nyears, ncol = N + 5)
     # Create a temporary matrix with 1 column
     Eps_s <- matrix(0, nrow = NROW(temp[[ar]]), ncol = 1)
     for (i in 1:NCOL(temp[[ar]])) {
       set.seed(i)
       # Bias correction added
       Eps_k = rnorm(NROW(temp[[ar]]), mean = 0, sd = tempcond[ar])
       Eps_s[1] <- Eps_k[1] * sigmar/tempcond[ar]
       for (t in 2:NROW(temp[[ar]])) {
         Eps_s[t] <- Eps_s[t - 1] * AR[ar] + Eps_k[t]
       }
       temp[[ar]][, i] <- Eps_s - sigmar^2 / 2
     }
   } # End AR loop
   sigmarecdev[[length(sigmarecdev) + 1]] <- temp

   message("exponentiated mean equals ",
     paste(round(sapply(temp, FUN = function(mat) {
       mean(exp(mat))
       }), 3), collapse = " "))
   message("marginal standard deviation equals ",
     paste(round(sapply(temp, FUN = function(mat) {
       sd(mat)
       }), 3), collapse = " "))
   message("autocorrelation equals ",
     paste(round(sapply(temp, FUN = function(mat) {
       mean(apply(mat, MARGIN = 2, FUN = function(vec){
         var(vec[-length(vec)], vec[-1]) / var(vec)
         })) }), 3), collapse = " "))

 } # End sigmar loop

for (sigmar in seq_along(my.sigmar)) {
  # Do not rerun for the simulation level sigmar of 0.4
  if (my.sigmar[sigmar] == SDmarg) next
  # Create OM and EM folders for each level of sigmar
  dir.create(sigmarspp[sigmar])
  system(paste("xcopy", my.spp, sigmarspp[sigmar],
        "/E /S /H /I"), show.output.on.console = verbose)

  # Fix par files
  ompar <- file.path(sigmarspp[sigmar], "om", "ss3.par")
  par <- readLines(ompar)
  par[grep("SR_parm\\[3\\]", par) + 1] <- my.sigmar[sigmar]
  writeLines(par, ompar)

  # Fix om control files
  r4ss::SS_changepars(dir = file.path(sigmarspp[sigmar], "om"),
    ctlfile = "ss3.ctl",
    newctlfile = "ss3.ctl",
    strings = "SR_sigmaR",
    newvals = my.sigmar[sigmar],
    estimate = FALSE,
    verbose = TRUE,
    newphs = NULL)

  # Fix em control files
  r4ss::SS_changepars(dir = file.path(sigmarspp[sigmar], "em"),
    ctlfile = "ss3.ctl",
    newctlfile = "ss3.ctl",
    strings = "SR_sigmaR",
    newvals = my.sigmar[sigmar],
    estimate = FALSE,
    verbose = TRUE,
    newphs = NULL)

  # Copy all of the case files with new species
  codfiles <- dir(case_folder, full.names = TRUE)
  done <- file.copy(codfiles, gsub(my.spp, sigmarspp[sigmar], codfiles))
} # End of sigmar loop


#' Run the simulation
for (sigmar in seq_along(my.sigmar)[-5]) {
for (ar in 4) { #1:length(AR)) {
  # Do not rerun for the simulation level sigmar of 0.4
  if (my.sigmar[sigmar] == SDmarg) next
    run_ss3sim(
      iterations = 1:N,
      scenarios = paste0("A101-D101-E120-F1-L0-", sigmarspp[sigmar]),
      user_recdevs = sigmarecdev[[sigmar]][[ar]],
      bias_adjust = FALSE,
      om_dir = file.path(sigmarspp[sigmar], "om"),
      em_dir = file.path(sigmarspp[sigmar], "em"),
      hess_always = TRUE,
      parallel = doparallel, parallel_iterations = doparallel,
      case_files = my.cases, case_folder = case_folder,
      user_recdevs_warn = verbose
    )

    sppname <- paste0(substr(sigmarspp[sigmar], 1, 3), letters[ar], "y")
    for (scen in paste0("A101-D101-E120-F1-L0-", sigmarspp[sigmar])) {
      # Copy to a new species name for the level of autocorrelation
      newscenario <- gsub(sigmarspp[sigmar], sppname, scen)
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
      system(paste("xcopy", newscenario, gsub(sigmarspp[sigmar], paste0(sppname, type), scen),
        "/E /S /H /I"), show.output.on.console = verbose)
      if (type == "t") {
        truear <- AR[ar]
      } else {
        truear <- NULL
      }
      # Use parallel processing to loop through each iteration
      foreach(it = 1:N) %dopar% {
        em(it = it, type = type, truear = truear, dir = dir.main,
        sppold = sigmarspp[sigmar], sppnew = sppname,
        scenario = scen, verbose = verbose)
      } # End loop over each iteration of alternative AR estimation
      if (type == "z") {
        system(paste("xcopy",
          gsub(sigmarspp[sigmar], paste0(sppname, "z"), scen),
          gsub(sigmarspp[sigmar], paste0(sppname, "zx"), scen), "/E /S /H /I"),
          show.output.on.console = verbose)
        foreach(it = 1:N) %dopar% {
          em(it = it,
            sppold = paste0(sppname, "z"),
            sppnew = paste0(sppname, "z"),
            scenario = gsub(sigmarspp[sigmar], paste0(sppname, "z"), scen),
            dir = dir.main, type = "x", truear = truear, verbose = verbose)
        }
      }
     } # End loop over each type of EM
   } # End loop over each scenario
  } # End loop over AR level
} # End loop over sigmar loop
