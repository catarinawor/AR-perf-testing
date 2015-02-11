################################################
#
# Performance testing SS3 with autocorrelated recruitment data.
#
# Authors (order subject to change): Councill, E., J. Thorson, L. Brooks, K. Johnson, R. Methot
#
# Date written: November 2, 2014
# Date last modified: January 013, 2015
# Last modifying author: EC
#
###############################################

# NOTES:
# (1) This code requires that all E0-cod through E5-cod text files be save 
#     into .../library/ss3sim/extdata/eg-cases folder after installing ss3sim from CRAN.  
# (2) Files E0-cod through E5-cod represent forecasts given for 0, 1, 3, 5, 10, and 20 years respectively.  
# (3) The set of AR values currently under consideration are: AR = 0, 0.25, 0.5, 0.75, 1.  
# (4) I'd like to hear any comments you may have about what is available below and/or on the project in general at any time.
# (5) To install from github, use devtools::install_github("ss3sim/ss3sim","master").  You will need to close and reopen R after 
#     installing the package from github.  install.packages("ss3sim") will give the CRAN version which, as to date, does not work 
#     with replicates and bias correction turned on.
# (6) From KFJ: I used ToEC to ask questions within the code, could you please look at these line numbers
#     to make sure I did not mess anything up.

###############################################

# If necessary install / or update ss3sim package
#devtools::install_github("ss3sim/ss3sim" "master") #beta
#install.packages("ss3sim") #CRAN

# Call necessary library packages
library("ss3sim")
library("r4ss")

# Set correct directories
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d,"/eg-cases")
om <- paste0(d,"/models/cod-om")
em <- paste0(d,"/models/cod-em")

# Write casefiles, this assumes your working directory is currently where you opened this file.
source("AR_generateCaseFiles.R")

# ss3sim requires the current working directory to be the package library folder in which ss3sim is installed
setwd('C:/Program Files/R/R-3.1.2/library/ss3sim')

# Generate rec devs for cod
SDmarg = 0.6
# Set level of autocorrelation
AR = 0.75

# Set number of iterations (replicates)
N = 20
# Set number of bias iterations
NB = 10 

# Set scenario names and classify which letters are used
# using my.cases allows the removal of M from the scenario names, which is
# only used for time-varying natural mortality
# D == data; F = fishing; R = retrospective run; E = number of forecast years
my.scenarios <- expand_scenarios(cases = list(D = 0, E = 0:5, F = 0, R = 0),
                                 species = "cod")
my.cases <- list(D = c("agecomp", "lcomp", "index"), E = "E", F = "F", R = "R")



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
    Eps <- matrix(0, nrow = 100, ncol = N)
    Eps_s <- matrix(0, nrow=100,ncol = 1)
    # Loop generate rec devs and save to single matrix, "Eps"
		for (i in 1:N) {
			set.seed(i)
			# Bias correction added
			Eps_k = rnorm(100, mean = -SDcond^2/2, sd = SDcond)
			for (t in 2:100) {
				Eps_s[1] <-Eps_k[1]
				Eps_s[t] <- Eps_s[t-1] * AR[arindex] + Eps_k[t]
			}
			Eps[, i] <- Eps_s
		}
		#OPTIONAL: Check for bias correction (uncomment below two lines)
		#bias_test<-rowMeans(exp(Eps))
		#plot(bias_test)
	for(bias in 0:1){
		#Run scenarios
		sppname <- paste0(letters[arindex], ifelse(bias == 0, "nb", "yb"))
		sppname.store[[counter]] <- sppname
		run_ss3sim(iterations = 1:N, scenarios = my.scenarios, case_files = my.cases,
    	case_folder = case_folder, om_dir = om, em_dir = em, 
	    bias_adjust = ifelse(bias == 0, FALSE, TRUE), bias_nsim = NB,
	    user_recdevs = Eps, show.output.on.console = FALSE)
        #Move results
        for(q in seq_along(my.scenarios)){
        	command <- paste("mv ", my.scenarios[q], gsub("cod", sppname, my.scenarios[q]))
        	system(command)
        }
	}
}

# Read in the results, no need to specify scenarios if you want the results for everything
# use overwrite_files = FALSE, if some results have already been read and you just want to update
# with the newest scenarios that were ran.
setwd("C:/Users/Elizabeth.Councill/Desktop/Main Project/COD/AR 0_5")
get_results_all(overwrite = FALSE)

