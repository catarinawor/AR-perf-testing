###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-01-08
####Purpose    : To create casefiles for ss3sim
####Packages   : ss3sim
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

###############################################################################
###############################################################################
#### Step
#### Make sure dependent objects are available
###############################################################################
###############################################################################
neededobjects <- c("case_folder", "my.forecasts", "my.biology", "my.spp","my.dats")

ignore <- sapply(neededobjects, function(x) {
    if (!exists(x)) {
    stop(paste0("\n\n Error message from KFJ:\n",
                "AR_generateCaseFiles.R should be called from the script",
                "AR_Simulations_KFJ.R.\n",
                "\"", x, "\" is part of AR_Simulations_KFJ.R, and was not found."))
}
})

###############################################################################
###############################################################################
#### Step
#### Generate case files and place them inside the eg-cases folder for ss3sim
###############################################################################
###############################################################################
if (!file.exists(case_folder)) {
  stop(paste("The casefile folder,", case_folder, " does not exist."))
}
wd.entry <- getwd()
setwd(case_folder)

start <- 1
end   <- 100

start.fishery <- 26

#start.survey  <- start + 75
# Changed to start the survey after the initial burn in period.
# You really should not have fishing or a survey until the model stabilizes
# which typically occurs round year 20 or so.
start.survey <- 25
start.fishery <- start + 25
freq.survey  <- 2
freq.fishery <- c(10, 4)
years.fish <- end + start - start.fishery

# Number of years the fishery ramps up before starting two way trip
all.surv <- seq(start.survey, end, by = freq.survey)
all.fish <- c(seq(start.fishery, start.fishery + 20, by = freq.fishery[1]),
              seq(start.fishery + 30, end, by = freq.fishery[2]))

# Information regarding sample intensity
high <- my.dats[2]
low <- my.dats[1]

# Information regarding CV
CV <- my.dats[3]

# Get F from the package
tofind <- paste0("F[0-9]+-", my.spp)
ffiles <- sapply(tofind, grep, value = TRUE,
  x = list.files(system.file("cases", package = "ss3models"), full.names = TRUE))
done <- file.copy(ffiles, ".", recursive = TRUE, overwrite = TRUE)
if (!any(done)) {
    stop(paste("Fishing case files were not properly copied."))
}

# Years of survey index of abundance
case_index(fleets = 2, case = 30, spp = my.spp, years = list(all.surv), sd = list(CV))

# Age comp

# Low data with fishery and survey and dirichlet
case_comp(fleets = 1:2,
  Nsamp = list(rep(low, length(all.fish)), rep(low, length(all.surv))),
  years = list(all.fish, all.surv), cpar = c(2,2),
  type = "agecomp", case = 30, spp = my.spp)

# Low data with fishery and sruvey and multinomial
case_comp(fleets = 1:2,
  Nsamp = list(rep(low, length(all.fish)), rep(low, length(all.surv))),
  years = list(all.fish, all.surv), cpar = c(1,1),
  type = "agecomp", case = 31, spp = my.spp)

# Low data case with only survey and dirichlet
case_comp(fleets = 2,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 2,
  type = "agecomp", case = 32, spp = my.spp)

# Low data case with only survey and multinomial
case_comp(fleets = 2,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 1,
  type = "agecomp", case = 33, spp = my.spp)

# Low data case with only fishery data and dirichlet
case_comp(fleets = 1,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 2,
  type = "agecomp", case = 34, spp = my.spp)

# Low data case with only fishery data and multinomial
case_comp(fleets = 1,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 1,
  type = "agecomp", case = 35, spp = my.spp)

# High data with fishery and sruvey and multinomial
case_comp(fleets = 1:2,
          Nsamp = list(rep(high, length(all.fish)), rep(high, length(all.surv))),
          years = list(all.fish, all.surv), cpar = c(1,1),
          type = "agecomp", case = 36, spp = my.spp)

# Length comp

# Low data with fishery and survey and dirichlet
case_comp(fleets = 1:2,
  Nsamp = list(rep(low, length(all.fish)), rep(low, length(all.surv))),
  years = list(all.fish, all.surv), cpar = c(2,2),
  type = "lcomp", case = 30, spp = my.spp)

# Low data with fishery and sruvey and multinomial
case_comp(fleets = 1:2,
  Nsamp = list(rep(low, length(all.fish)), rep(low, length(all.surv))),
  years = list(all.fish, all.surv), cpar = c(1,1),
  type = "lcomp", case = 31, spp = my.spp)

# Low data case with only survey and dirichlet
case_comp(fleets = 2,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 2,
  type = "lcomp", case = 32, spp = my.spp)

# Low data case with only survey and multinomial
case_comp(fleets = 2,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 1,
  type = "lcomp", case = 33, spp = my.spp)

# Low data case with only fishery data and dirichlet
case_comp(fleets = 1,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 2,
  type = "lcomp", case = 34, spp = my.spp)

# Low data case with only fishery data and multinomial
case_comp(fleets = 1,
  Nsamp = list(rep(low, length(all.surv))),
  years = list(all.surv), cpar = 1,
  type = "lcomp", case = 35, spp = my.spp)

# High data with fishery and sruvey and multinomial
case_comp(fleets = 1:2,
          Nsamp = list(rep(high, length(all.fish)), rep(high, length(all.surv))),
          years = list(all.fish, all.surv), cpar = c(1,1),
          type = "lcomp", case = 36, spp = my.spp)

## Generate casefiles for different lengths of forecasting using E
for (f in 1:length(my.forecasts)){
    file.current <- paste0("E", f, "-", my.spp, ".txt")
    mapply(writeLines, con = file.current, MoreArgs = list(c(
    "# description: Fixed M, no qSurvey, w a given forecast number",
    "natM_type; 1Parm",
    "natM_n_breakpoints; NULL",
    "natM_lorenzen; NULL",
    "natM_val; c(NA, -1)",
    "par_name; NULL",
    "par_int; NA",
    "par_phase; NULL",
    paste("forecast_num;", my.forecasts[2])))
    )
}

# E change_e, change parameter estimation
writeE <- function(name, int, phase, forecast, species, case) {
sink(paste0("E", case, "-", species, ".txt"))
    cat("natM_type; 1Parm \nnatM_n_breakpoints; NULL \n",
        "natM_lorenzen; NULL \n", sep = "")
    testfornatm <- grepl("NatM_p_1_Fem", name)
    if (any(testfornatm)) {
        cat("natM_val; c(", int[testfornatm], ", ", phase[testfornatm],
            ")\n", sep = "")
    } else {
      cat("natM_val; c(NA, -1) \n", sep = "")
    }
    if (all(testfornatm)) {
        cat("par_name; NULL \n", "par_int; NULL \n", "par_phase; NULL \n", sep = "")
      } else{
        cat("par_name; c(\"",
            paste0(name[!testfornatm], collapse = "\", \""), "\")\n", sep = "")
        if (all(is.na(int))) {
            cat("par_int; c(",
            paste0(int[!testfornatm], collapse = ", "), ")\n",
            "par_phase; c(",
            paste0(phase[!testfornatm], collapse = ", "), ")\n",
            sep = "")
        } else {
            cat("par_int; c(\"",
            paste0(int[!testfornatm], collapse = "\", \""), "\")\n",
            "par_phase; c(\"",
            paste0(phase[!testfornatm], collapse = "\", \""), "\")\n",
            sep = "")
        }
      }
      cat("forecast_num; ", forecast, sep = "")
  sink()
}

# Par phases are in the same order as the parameter vector
# If you move parameter values around you must also move values in the parameter phases around
# Top line are parameters that one wants turned off
# Second line are parameters that remain estimated in the EM
# MAKE SURE that there are consecutive positive parameter values otherwise SS will not run
# Turn off R0, K, Survey Selectivity
writeE(c(
  # Not estimated
  "SR_LN(R0)", "VonBert_K_Fem_GP_1", "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey",
  # Estimated
  "L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1", "LnQ_base_2_SURVEY", "SizeSel_1P_1_Fishery", "SizeSel_1P_3_Fishery"),
  rep(NA, 11),
  # The following line must contain digits 1-5 at least once.
  c(-1, -4, -2, -3, 1, 4, 5, 5, 5, 2, 3),
  my.forecasts[1], my.spp, 100)

# Turn off R0, K, Survey Selectivity, Fishery Selectivity
writeE(c(
  # Not estimated
  "SR_LN(R0)", "VonBert_K_Fem_GP_1", "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey", "SizeSel_1P_1_Fishery", "SizeSel_1P_3_Fishery",
  # Estimated
  "L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1", "LnQ_base_2_SURVEY"),
  rep(NA, 11),
  # The following line must contain positive values that do not skip numbers
  c(-1, -4, -2, -3, -2, -3, 1, 2, 3, 5, 5),
  my.forecasts[1], my.spp, 101)

writeE(c(
  # Not estimated
  "SR_LN(R0)", "VonBert_K_Fem_GP_1", "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey", "SizeSel_1P_1_Fishery", "SizeSel_1P_3_Fishery","L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1",
  # Estimated
  "CV_young_Fem_GP_1", "CV_old_Fem_GP_1", "LnQ_base_2_SURVEY"),
  rep(NA, 11),
  # The following line must contain positive values that do not skip numbers
  c(-1, -4, -2, -3, -2, -3, -2, -2, 1, 2, 3),
  my.forecasts[1], my.spp, 102)

writeE(c(
  # Not estimated
  "VonBert_K_Fem_GP_1", "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey", "SizeSel_1P_1_Fishery", "SizeSel_1P_3_Fishery","L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1",
  # Estimated
  "SR_LN(R0)", "LnQ_base_2_SURVEY"),
  rep(NA, 11),
  # The following line must contain positive values that do not skip numbers
  c(-1, -4, -2, -3, -2, -3, -2, -2, -1, 1, 1),
  my.forecasts[1], my.spp, 103)

writeE(c(
  # Not estimated
  "LnQ_base_2_SURVEY",
  # Estimated
  "SR_LN(R0)", "VonBert_K_Fem_GP_1", "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey", "SizeSel_1P_1_Fishery","SizeSel_1P_3_Fishery","L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1"),
  rep(NA, 11),
  # The following line must contain positive values that do not skip numbers
  c(-1, 4, 2, 3, 2, 5, 2, 2, 1, 1, 1),
  my.forecasts[1], my.spp, 104)

writeE(c(
  # Not estimated
  "LnQ_base_2_SURVEY",  "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey",
  # Estimated
  "SR_LN(R0)", "VonBert_K_Fem_GP_1", "SizeSel_1P_1_Fishery","SizeSel_1P_3_Fishery","L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1"),
  rep(NA, 11),
  # The following line must contain positive values that do not skip numbers
  c(-1, -2, -4, 3, 2, 5, 4, 2, 1, 1, 1),
  my.forecasts[1], my.spp, 105)

writeE(c(
  # Not estimated

  # Estimated
  "LnQ_base_2_SURVEY",  "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey","SR_LN(R0)", "VonBert_K_Fem_GP_1", "SizeSel_1P_1_Fishery","SizeSel_1P_3_Fishery","L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1"),
  rep(NA, 11),
  # The following line must contain positive values that do not skip numbers
  c(1, 2, 3, 3, 2, 5, 4, 2, 1, 1, 1),
  my.forecasts[1], my.spp, 106)

writeE(c(
  # Not estimated

  # Estimated
  "LnQ_base_2_SURVEY",  "SizeSel_2P_1_Survey", "SizeSel_2P_3_Survey","SR_LN(R0)", "VonBert_K_Fem_GP_1", "SizeSel_1P_1_Fishery","SizeSel_1P_3_Fishery","L_at_Amin_Fem_GP_1", "L_at_Amax_Fem_GP_1", "CV_young_Fem_GP_1", "CV_old_Fem_GP_1"),
  rep(NA, 11),
  # The following line must contain positive values that do not skip numbers
  c(1, 2, 3, 3, 2, 5, 4, 2, 1, 1, 1),
  my.forecasts[2], my.spp, 107)


# file.current <- paste0("E",100,"-",my.spp,".txt")
# mapply(writeLines,con=file.current, MoreArgs = list(c(
#   "# description: Fixed M, no qSurvey, w a given forecast number, est sigmaR",
#   "natM_type; 1Parm",
#   "natM_n_breakpoints; NULL",
#   "natM_lorenzen; NULL",
#   "natM_val; c(NA, -1)",
#   "par_name; c(SR_LN(R0),VonBert_K_Fem_GP_1,SizeSel_2P_1_Survey,SizeSel_1P_3_Fishery)",
#   "par_int; c(NA,NA,NA,NA)",
#   "par_phase; c(-1,-1,-1,-1)",
#   paste("forecast_num;", my.forecasts[1])))
# )

## Generate casefiles for different lengths of forecasting using E
for(f in 1:length(my.forecasts)){
    file.current <- paste0("E", f + 10, "-", my.spp, ".txt")
    mapply(writeLines, con = file.current, MoreArgs = list(c(
    "# description: Fixed M, no qSurvey, w a given forecast number, est sigmaR",
    "natM_type; 1Parm",
    "natM_n_breakpoints; NULL",
    "natM_lorenzen; NULL",
    "natM_val; c(NA, -1)",
    "par_name; SR_sigmaR",
    "par_int; NA",
    "par_phase; 5",
    paste("forecast_num;", my.forecasts[1])))
    )
}

## Generate casefiles for different levels of age at 50% maturity
for (spp in seq_along(my.spp)) {
  for(b in 1:ncol(my.biology)) {
    file.current <- file(paste0("B", b-1, "-", my.spp[spp], ".txt"), open = "w")
    writeLines(c(
    "# A case file to change the age at 50% maturity",
    "function_type; change_tv",
    "param; Mat50%_Fem",
    paste0("dev; rep(", my.biology[spp, b], ", 100)\n")), file.current)
    close(file.current)
  }
  # Do not need to change the em if using the original biology
  if (b == 1) next
  ctl <- SS_parlines(file.path("..", my.spp[spp], "om", "ss3.ctl"))
  for(f in 1:length(my.forecasts)){
    file.current <- file(paste0("E", f + 20, "-", my.spp[spp], ".txt"), open = "w")
    writeLines(c(
    "# description: Fixed M, no qSurvey, w a given forecast number, change mat",
    "natM_type; 1Parm",
    "natM_n_breakpoints; NULL",
    "natM_lorenzen; NULL",
    "natM_val; c(NA, -1)",
    "par_name; Mat50%_Fem",
    paste("par_int;", ctl[ctl$Label == "Mat50%_Fem", "INIT"] + my.biology[spp, b]),
    "par_phase; NA",
    paste("forecast_num;", my.forecasts[1])), file.current)
    close(file.current)
  }
}

setwd(wd.entry)
