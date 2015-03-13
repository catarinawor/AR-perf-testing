################################################
#
# Performance testing SS3 with autocorrelated recruitment data.
#
# Authors (order subject to change): Councill, E., J. Thorson, L. Brooks, K. Johnson, R. Methot
#
# Date written: November 2, 2014
# Date last modified: January 22, 2015
# Last modifying author: KFJ
#
###############################################

# NOTES:
# (1) This code requires that all E0-cod through E5-cod text files be save
#     into .../library/ss3sim/extdata/eg-cases folder after installing ss3sim from CRAN.
# (2) Files E1-cod through E6-cod represent forecasts given for 0, 1, 3, 5, 10, and 20 years respectively.
# (3) The set of AR values currently under consideration are: AR = 0, 0.25, 0.5, 0.75, 1.
# (4) I'd like to hear any comments you may have about what is available below and/or on the project in general at any time.
# (5) To install from github, use devtools::install_github("ss3sim/ss3sim","master").  You will need to close and reopen R after
#     installing the package from github.  install.packages("ss3sim") will give the CRAN version which, as to date, does not work
#     with replicates and bias correction turned on.
# (6) The bias adjustment runs were just running in the folder that had already ran the iterations.
#     I changed this now so that each folder is copied to the "copies" directory before the
#     bias adjustment scenario starts.
# (7) Changed the number of recruitment deviations generated because the bias adjustment runs use up some of the iterations.

###############################################

# If necessary install / or update ss3sim package
devtools::install_github("ss3sim/ss3sim", "master") #beta
devtools::install_github("ss3sim/ss3models", "master") #beta
devtools::install_github("r4ss/r4ss", "master") #beta

#install.packages("ss3sim") #CRAN

# Call necessary library packages
library("ss3sim")
library("ss3models")
library("r4ss")

# To run in parallel
library(doParallel)
registerDoParallel(cores = 3)
library(foreach)
doparallel <- TRUE

# Set correct directories
case_folder <- file.path(getwd(),"cases")
dir.create(case_folder, recursive = TRUE, showWarnings = FALSE)
file.copy(system.file("cases", package = "ss3models"), ".", recursive = TRUE)

file.copy(system.file("models", "cod", "om", package = "ss3models"), ".", recursive = TRUE)
file.copy(system.file("models", "cod", "em", package = "ss3models"), ".", recursive = TRUE)
om <- "om"
em <- "em"
wd.copy <- "copies"
dir.create(wd.copy, showWarnings = FALSE)

#Fix the bias adjustment line in EM such that no bias adjustment is run
# i.e., set it equal to 1, which uses pre-2009 SS methods
emctl <- readLines(file.path(em, "ss3.ctl"))
changeline <- grep("#_max_bias_adj_in_MPD", emctl)
biasline <- strsplit(emctl[changeline], "#")[[1]][2]
emctl[changeline] <- paste(-1, biasline, sep = " #")
writeLines(emctl, file.path(em, "ss3.ctl"))

# Generate rec devs for cod
SDmarg = 0.6
# Set level of autocorrelation
AR = c(0, 0.25, 0.5, 0.75, 0.9)

# Set number of iterations (replicates)
N = 101
# Set number of bias iterations
NB = 20

# Set number of forecast years
my.forecasts <- c(0, 1, 3, 5, 10, 20)
# change the age at 50% maturity from the true value
my.biology <- c(0, -10.0)

# Set the following line to TRUE if you want to run a short test
# to make sure everything works.
if (FALSE) {
    AR <- tail(AR, 2)
    my.forecasts <- head(my.forecasts, 2)
    N <- 2
    NB <- 2
}

# Write casefiles, this assumes your working directory is currently
# where you opened this file.
source("generateCaseFiles.R")

# Set scenario names and classify which letters are used
# using my.cases allows the removal of M from the scenario names, which is
# used for time-varying natural mortality
# D == data; F = fishing; R = retrospective run; E = number of forecast years
my.scenarios <- expand_scenarios(cases = list(D = 30, E = 1:length(my.forecasts),
                                 F = 0), species = "cod")
my.cases <- list(D = c("agecomp", "lcomp", "index"), E = "E", F = "F")

# Run ss3sim using prescribed rec devs
#
# NOTE: If you wish to run ss3sim more than once, you will need to rename or delete the previous run's output folders.
# To turn off cloning of input add argument "printstats = FALSE"
# To:EC can you look at this for loop and see if the seed is still being set properly.
# This gives the same seed for the each iteration of the different AR levels
sppname.store <- list(); counter <- 0
for(arindex in seq_along(AR)){
    counter <- counter + 1
    SDcond = SDmarg *sqrt(1 - AR[arindex])
    # Set matrix of rec devs as "Eps"
    # Matrix must have one row for every year in the simulation
    # and one column for every iteration I would think that
    Eps <- matrix(0, nrow = 100, ncol = N+NB)
    #Eps <- matrix(0, nrow = 100, ncol = N)
    Eps_s <- matrix(0, nrow = NROW(Eps), ncol = 1)
    # Loop generate rec devs and save to single matrix, "Eps"
        for (i in 1:NCOL(Eps)) {
            set.seed(i)
            # Bias correction added
            Eps_k = rnorm(NROW(Eps), mean = -SDcond^2/2, sd = SDcond)
            for (t in 2:NROW(Eps)) {
                Eps_s[1] <-Eps_k[1]
                Eps_s[t] <- Eps_s[t-1] * AR[arindex] + Eps_k[t]
            }
            Eps[, i] <- Eps_s
        }
        #OPTIONAL: Check for bias correction (uncomment below two lines)
        #bias_test<-rowMeans(exp(Eps))
        #plot(bias_test)
    for(bias in 0:1){
        # If bias == 0, no bias adjustment is performed
        # If bias == 1, bias adjustment routines are done prior to the iterations
        # Run scenarios
        # Scenarios will be renamed based on whether or not bias adjustment was run
        sppname <- paste0(letters[arindex], ifelse(bias == 0, "nb", "yb"))
        sppname.store[[counter]] <- sppname
        run_ss3sim(iterations = 1:N, scenarios = my.scenarios, case_files = my.cases,
        case_folder = case_folder, om_dir = om, em_dir = em,
        bias_adjust = ifelse(bias == 0, FALSE, TRUE), bias_nsim = NB,
        user_recdevs = Eps, user_recdevs_warn = FALSE, show.output.on.console = FALSE,
        parallel = doparallel, parallel_iterations = doparallel)

        # Move results
        # For each scenario move the results to the folder copies and change the name
        for(q in seq_along(my.scenarios)){
            file.rename(my.scenarios[q],
                        file.path("copies", gsub("cod", sppname, my.scenarios[q])))
            unlink(my.scenarios[q], recursive = TRUE)
        }
    }
}

# Read in the results, no need to specify scenarios if you want the results for everything
# use overwrite_files = FALSE, if some results have already been read and you just want to update
# with the newest scenarios that were ran.
setwd("copies")
get_results_all(overwrite = FALSE)

