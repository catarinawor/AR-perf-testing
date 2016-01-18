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
 library(ggplot2)
 library(knitr) # To compile output
 library(xtable)

# Variable inputs
 doparallel <- TRUE
 AR = c(-0.25, 0, 0.25, 0.5, 0.75, 0.9) # levels of autocorrelation
 N = 100 # number of replicates
 nyears <- 100 # length of simulation
 burnin <- 25 # length of burnin period
 my.dats <- c(100, 2000, 0.1, 25) # amount of data (low, high, CV, lower)
 my.forecasts <- c(20) # number of forecast years
 my.spp <- c("cod") # spp to include in the simulation
 verbose <- FALSE # show warnings and information
 switch <- 0 # EM starter; 0 == default phases, 1 == phase of -1
 resolution <- 100
 width <- 600
 height <- 600
 unit <- "px"
 types <- c("z", "t", "x") # EM types, see "em"
 # no label -- estimate internally
 # t -- fixed at true value
 # x -- estimate externally and then fix
 # z -- fix at zero

if (length(my.spp) > 1| length(my.forecasts) > 1) {
  stop("some code needs to be re-written to accommodate more species or forecasts")
}
# Manage working directories
if (Sys.getenv("USERNAME") == "Elizabeth.Councill") {
  dir.main <- "C:/Users/Elizabeth.Councill/Desktop/AR-perf-testing/AR-perf-testing/new/"

}
if (Sys.getenv("USERNAME") == "kelli") {
  dir.main <- "C:/AR-perf-testing"
  if(!file.exists(dir.main)) {
    dir.main <- "T:/AR-perf-testing"
  }
}
if (Sys.getenv("USERNAME") == "kfjohns") {
  dir.main <- "T:/AR-perf-testing"
}
if (Sys.getenv("USERNAME") == "James.Thorson") {
  dir.main <- paste0("C:/Users/James.Thorson/Desktop/Project_git/AR-perf-testing")
  # ALSO PUT MODIFIED EXE IN: C:\Users\James.Thorson\Documents\R\R-3.2.0\library\ss3sim\bin\Windows64
}
 case_folder <- file.path(dir.main, "cases")
 fig_folder <- file.path(dir.main, "figures")

 dir.create(case_folder, recursive = TRUE, showWarnings = verbose)
 dir.create(fig_folder, recursive = TRUE, showWarnings = verbose)
 setwd(dir.main)
 done <- file.copy(system.file("models", my.spp, package = "ss3models"), ".",
   recursive = TRUE)

# Source functions
ignore <- mapply(source, list.files("R", full.names = TRUE))

# Set up parallel running
if (doparallel) {
  numcores <- Sys.getenv("NUMBER_OF_PROCESSORS")
  mode(numcores) <- "numeric"
  numcores <- numcores - 1
  cl <- makeCluster(numcores)
  registerDoParallel(cl)
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
   emctl[changeline] <- "-0.99 0.99 0 0 -1 0 5 # SR_autocorr"
   # Change forecast file
   changeline <- grep("#_MSY", emfor)
   emfor[changeline] <- gsub("[0-9]+", 4, emfor[changeline])
   changeline <- grep("#_FirstYear_for_caps_and_allocations", emfor)
   emfor[changeline] <- gsub("[0-9]+", nyears + 1, emfor[changeline])
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

# Copy executable into ss3sim bin
binfolder <- dirname(get_bin())
ss3simtpl <- list.files(binfolder, full.names = TRUE)
ss3simnew <- file.path(dirname(ss3simtpl), c("opt", "safe"))
ignore <- file.rename(ss3simtpl, ss3simnew)
goodtpl <- list.files(file.path(dir.main, "tpl", "V4"),
  pattern = "\\.exe", full.names = TRUE)
ignore <- file.copy(goodtpl, ss3simtpl)

my.cases <- list(D = "index", A = "agecomp", L = "lcomp", E = "E", F = "F")
lag <- 1 # Lag used for external estimate of autocorrelated rec devs
timeframe <- c(burnin + 1, nyears - my.forecasts) # Time frame to use for external estimate
ncols <- 300 # number of columns for Report.sso file

 for (spp in my.spp) {
   # Create recruitment deviations
   # A list is generated with one matrix per level of AR
   SDmarg <- get_sigmar(file.path(spp, "om", "ss3"))
   SDcond = SDmarg * sqrt(1 - AR^2)     # BUG #1
   EpsList <- list()
   for (ar in seq_along(AR)) {
     EpsList[[ar]] <- matrix(0, nrow = nyears, ncol = N + 5)
     # Create a temporary matrix with 1 column
     Eps_s <- matrix(0, nrow = NROW(EpsList[[ar]]), ncol = 1)
     for (i in 1:NCOL(EpsList[[ar]])) {
       set.seed(i)
       # Bias correction added
       Eps_k = rnorm(NROW(EpsList[[ar]]), mean = 0, sd = SDcond[ar])
       Eps_s[1] <- Eps_k[1] * SDmarg/SDcond[ar]           # BUG #2
       for (t in 2:NROW(EpsList[[ar]])) {
         Eps_s[t] <- Eps_s[t - 1] * AR[ar] + Eps_k[t]     # BUG #3
       }
       EpsList[[ar]][, i] <- Eps_s - SDmarg^2 / 2         # BUG #4
     }
   }

   # NEW SANITY CHECKS (actually, I decided to just make it a message)
   sanity_check = function(vec, num, rel.tol=0.01, infl=0, failmessage="Sanity check failed"){
     if( any(abs(vec-num)/(num+infl)>rel.tol) ) stop(failmessage)
   }
   message("exponentiated mean equals ",paste(round(sapply(EpsList, FUN=function(mat){ mean(exp(mat)) }),3),collapse=" "))
   message("marginal standard deviation equals ",paste(round(sapply(EpsList, FUN=function(mat){ sd(mat) }),3),collapse=" "))
   message("autocorrelation equals ",paste(round(sapply(EpsList, FUN=function(mat){ mean(apply(mat,MARGIN=2,FUN=function(vec){var(vec[-length(vec)],vec[-1])/var(vec)})) }),3),collapse=" "))
   #

   source("AR_recruitmentplot.R")
   # End of recruitment deviations for each species
 } # End species loop

# Run script which fixes steepness at 1.0
source("AR_steepness.R", echo = verbose)
source("AR_power.R", echo = verbose)
source("AR_lengthdata.R", echo = verbose)
source("AR_sigmar.R", echo = verbose)

if (doparallel) stopCluster(cl)

detach("package:ss3sim", unload = TRUE)
devtools::install_github("ss3sim/ss3sim@derivedquant") #use to get results
library(ss3sim)
get_results_all()

source("AR_results.R", echo = verbose)
