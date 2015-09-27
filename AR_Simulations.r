################################################
#
# Title: Performance testing SS3 with autocorrelated recruitment data.
#
# Authors (order subject to change): Councill, E., J. Thorson, L. Brooks, K. Johnson, R. Methot
#
# Date written: November 2, 2014
# Date last modified: September 23, 2015
# Last modifying author: KFJ
#
###############################################

# Packages
devtools::install_github("ss3sim/ss3sim@master") #current
devtools::install_github("ss3sim/ss3models@master")
devtools::install_github("r4ss/r4ss@master")

 library("ss3sim")
 library("ss3models")
 library("r4ss")
 library(doParallel) #For parallel
 library(foreach)
 library(knitr) # To compile output

# Variable inputs
 doparallel <- FALSE
 SDmarg <- 0.6 # rec devs
 AR = c(-0.25, 0, 0.25, 0.5, 0.75, 0.9) # levels of autocorrelation
 N = 3 # number of replicates
 NB = 1 # number of bias adjustment runs
 nyears <- 100 # length of simulation
 burnin <- 25 # length of burnin period
 my.dats <- c(100, 2000, 0.1) # amount of data (low, high, CV)
 my.forecasts <- c(20) # number of forecast years
 my.spp <- c("cod") # spp to include in the simulation
 verbose <- FALSE # show warnings and information
 switch <- 0 # EM starter; 0 == default phases, 1 == phase of -1
 resolution <- 100
 width <- 600
 height <- 600
 unit <- "px"

if (length(my.spp) > 1| length(my.forecasts) > 1) {
  stop("some code needs to be re-written to accommodate more species or forecasts")
}
# Manage working directories
if (Sys.getenv("USERNAME") == "Elizabeth.Councill") {
  dir.main <- "C:/Users/Elizabeth.Councill/Desktop/AR-perf-testing/AR-perf-testing/new/"

}
if (Sys.getenv("USERNAME") == "kelli") {
  dir.main <- "C:/AR-perf-testing"
  if(!exists(dir.main)) {
    dir.main <- "T:/AR-perf-testing"
  }
}
if (Sys.getenv("USERNAME") == "kfjohns") {
  dir.main <- "T:/AR-perf-testing"
}
 setwd(dir.main)
 case_folder <- file.path(dir.main, "cases")
 fig_folder <- file.path(dir.main, "figures")

 dir.create(case_folder, recursive = TRUE, showWarnings = verbose)
 dir.create(fig_folder, recursive = TRUE, showWarnings = verbose)
 done <- file.copy(system.file("models", my.spp, package = "ss3models"), ".",
   recursive = TRUE)

# Set up parallel running
if (doparallel) {
  numcores <- Sys.getenv("NUMBER_OF_PROCESSORS") - 1
  registerDoParallel(cores = numcores)
}

# OM and EM files for this simulation
# Change forecast file to use F == 0 and catches == 0
 for (spp in my.spp) {
   emctl <- readLines(file.path(spp, "em", "ss3.ctl"))
   emfor <- readLines(file.path(spp, "em", "forecast.ss"))
   emsta <- readLines(file.path(spp, "em", "starter.ss"))
   # Change EM ctl file bias adjustment line
   # such that no bias adjustment is run
   # -1 uses pre-2009 SS methods
   changeline <- grep("#_max_bias_adj_in_MPD", emctl)
   biasline <- strsplit(emctl[changeline], "#")[[1]][2]
   emctl[changeline] <- paste(-1, biasline, sep = " #")
   # Turn on autocorrelation estimation in em
   changeline <- grep("# SR_autocorr", emctl)
   emctl[changeline] <- "-1 1 0 0 -1 0 5 # SR_autocorr"
   # Change forecast file
   changeline <- grep("#_MSY", emfor)
   emfor[changeline] <- gsub("[0-9]+", 4, emfor[changeline])
   changeline <- grep("#_Forecast", emfor)
   emfor[changeline] <- gsub("[0-9]+", 2, emfor[changeline])
   changeline <- grep("#_First_forecast_loop_with_stochastic_recruitment", emfor)
   emfor[changeline] <- gsub("[0-9]+", 1, emfor[changeline])
   changeline <- grep("Ncatch", emfor)
   emfor[changeline] <- paste(my.forecasts, "#_Ncatch")
   emfor[changeline + 1] <- "2 #_InputBasis"
   catchdata <- data.frame("years" = (nyears - my.forecasts + 1):nyears,
     "seas" = 1, "fleet" = 1, "catch" = 0)
   catchdata <- apply(catchdata, 1, paste, collapse = " ")
   emfor <- append(emfor, catchdata, after = changeline + 1)
   if (switch == 1){
     changeline <- grep("# Turn off estimation for parameters entering after this phase", emsta)
     emsta[changeline] <- paste(-1, strsplit(emsta[changeline],"#")[[1]][2], sep = "#")
     writeLines(emsta, file.path(spp, "em", "starter.ss"))
   }
   writeLines(emctl, file.path(spp, "em", "ss3.ctl"))
   writeLines(emfor, file.path(spp, "em", "forecast.ss"))
 }

# Write casefiles, this assumes your working directory is currently
source("generateCaseFiles.R")

# Set scenario names and classify which letters are used
# D == data; F = fishing; E = number of forecast years
my.scenarios <- expand_scenarios(cases = list(D = 0, E = my.forecasts,
  A = my.dats[1:2], L = my.dats[1:2], F = 1), species = my.spp)
my.cases <- list(D = "index", A = "agecomp", L = "lcomp", E = "E", F = "F")
lag <- 1 # Lag used for external estimate of autocorrelated rec devs
timeframe <- c(burnin + 1, nyears - my.forecasts) # Time frame to use for external estimate
checkfiles <- c("CompReport.sso", "covar.sso",
  "Forecast-report.sso", "wtatage.ss_new") # Files to check if run converged
ncols <- 300 # number of columns for Report.sso file

 for (spp in my.spp) {
   # Create recruitment deviations
   # A list is generated with one matrix per level of AR
   SDmarg <- readLines(file.path(spp, "om", "ss3.ctl"))
   SDmarg <- grep("SR_sigmaR", SDmarg, value = TRUE)
   SDmarg <- strsplit(SDmarg, " ")[[1]]
   SDmarg <- SDmarg[!SDmarg == ""][3]
   mode(SDmarg) <- "numeric"
   SDcond = SDmarg * sqrt(1 - AR)
   EpsList <- list()
   for (ar in seq_along(AR)) {
     EpsList[[ar]] <- matrix(0, nrow = nyears, ncol = N + NB)
     # Create a temporary matrix with 1 column
     Eps_s <- matrix(0, nrow = NROW(EpsList[[ar]]), ncol = 1)
     for (i in 1:NCOL(EpsList[[ar]])) {
       set.seed(i)
       # Bias correction added
       Eps_k = rnorm(NROW(EpsList[[ar]]), mean = 0, sd = SDcond[ar])
       for (t in 2:NROW(EpsList[[ar]])) {
         Eps_s[1] <- Eps_k[1]
         Eps_s[t] <- Eps_s[t - 1] * AR[ar] + sqrt(1 - AR[ar]^2) * Eps_k[t]
       }
       EpsList[[ar]][, i] <- Eps_s - SDcond[ar]^2 / 2
     }
   }

   png(file.path(fig_folder, paste0(spp, "recdevs_iteration001.png")),
     width = width, height = height, res = resolution, units = unit)
   toplot <- sapply(EpsList, function(x) x[, 1])
   matplot(toplot, type = "l", las = 1, lty = 1:length(AR), col = "black",
     xlab = "year", ylab = "recruitment deviations")
   legend("topleft", legend = as.character(format(AR, digits = 2)),
     lty = 1:length(AR), bty = "n")
   dev.off()

   png(file.path(fig_folder, paste0(spp, "biasadjustmentcheck.png")),
     width = width, height = height, res = resolution, units = unit)
     par(mfrow = c(2, 1), mar = c(0, 4, 0, 0.25), oma = c(5, 1, 1, 1))
     matplot(sapply(EpsList, colMeans), ylim = c(-0.25, 1.25), xaxt = "n",
       pch = 21, ylab = "mean rec dev", col = "black")
     abline(h = 0)
     matplot(sapply(lapply(EpsList, exp), colMeans), ylim = c(-0.25, 1.25),
       ylab = "mean exponentiated rec dev", pch = 21, col = "black")
     abline(h = 1)
     mtext(side = 1, line = 2.5, "iteration")
   dev.off()
   # End of recruitment deviations for each species

   for (ar in seq_along(AR)) {
   for (bias in NB) {
      run_ss3sim(
        iterations = 1:N,
        scenarios = my.scenarios,
        user_recdevs = EpsList[[ar]],
        bias_nsim = bias,
        bias_adjust = ifelse(bias == 0, FALSE, TRUE),
        om_dir = file.path(spp, "om"), em_dir = file.path(spp, "em"),
        hess_always = TRUE, parallel = doparallel,
        case_files = my.cases, case_folder = case_folder,
        user_recdevs_warn = verbose, show.output.on.console = verbose,
      )
   truename <- grep(spp, my.scenarios, value = TRUE)
   sppname <- paste0(substr(spp, 1, 1), letters[ar], ifelse(bias == 0, "n", "y"))
   for (scen in truename) {
     # Copy to a new species name for the level of autocorrelation
     newscenname <- gsub(spp, sppname, scen)
     file.rename(scen, newscenname)
     # Copy and set AR to zero
     thisname <- gsub(spp, paste0(sppname, "z"), scen)
     system(paste("xcopy", newscenname, thisname, "/E /S /H /I"),
       show.output.on.console = verbose)
     currentwd <- getwd()
     for (it in 1:N) {
       setwd(file.path(thisname, it, "em"))
       emctl <- readLines("em.ctl")
       changeline <- grep("# SR_autocorr", emctl)
       emctl[changeline] <- "-1 1 0 0 -1 0 -5 # SR_autocorr"
       writeLines(emctl, "em.ctl")
       ignore <- system(get_bin(), show.output.on.console = verbose)
       setwd(currentwd)
     } # End loop over iterations for AR == 0 in EM
     # Copy and set AR to zero
     thisname <- gsub(spp, paste0(sppname, "x"), scen)
     system(paste("xcopy", newscenname, thisname, "/E /S /H /I"),
       show.output.on.console = verbose)
     setwd(thisname)
     iters <- dir(pattern = "[0-9]+$")
     #' Work through each iteration
     for (it in seq_along(iters)) {
       setwd(file.path(iters[it], "em"))
       grad <- readLines("Report.sso", n = 18)
       grad <- as.numeric(strsplit(grep("Convergence_Level:", grad, value = TRUE),
         "[[:space:]]")[[1]][2])
       converged <- grad < 0.01
       if (!converged) {
         setwd("../..")
         unlink(it, recursive = TRUE)
         next
       } # End if for non-convergence
     #' Check to see if certain files exist for obtaining the results
     checkexists <- mapply(file.exists, checkfiles)
     results <- SS_output(dir = getwd(), ncols = ncols,
       covar = checkexists["CompReport.sso"],
       forecast = FALSE,
       NoCompOK = !checkexists["CompReport.sso"],
       warn = verbose, verbose = verbose, printstats = verbose)
       recruits <- results$timeseries$Recruit_0
       r0 <- results$parameters[
       results$parameters$Label == "SR_LN(R0)", "Value"]
     recdev <- results$parameters[
       grep("Main_RecrDev_", results$parameters$Label), "Value"]
     # Calculate the autocorrelation
     forss <- acf(recdev[timeframe[1]:timeframe[2]], plot = FALSE,
       na.action = na.omit)[lag]$acf[1]
     ss3ctl <- readLines(list.files(pattern = "ctl$"))
     changeline <- grep("SR_autocorr", ss3ctl)
     ss3ctl[changeline] <- paste("-1 1", forss, "0 -1 0 -5 # SR_autocorr")
     writeLines(ss3ctl, list.files(pattern = "ctl$"))
       ignore <- system(get_bin(), show.output.on.console = verbose)
     setwd("../..")
   } # End loop over iterations for AR == external estimate
   setwd(currentwd)
   } # End loop over each scenario
   } # End bias adjustment number loop
   } # End loop over AR level
 } # End species loop

detach("package:ss3sim", unload = TRUE)
devtools::install_github("ss3sim/ss3sim@derivedquant") #use to get results
library(ss3sim)
get_results_all()
