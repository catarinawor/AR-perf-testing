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
neededobjects <- c("case_folder", "my.forecasts", "my.biology", "my.spp")

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

start.survey  <- start + 75
start.fishery <- start + 25
freq.survey  <- 2
freq.fishery <- c(10, 4)
years.fish <- end + start - start.fishery

# Number of years the fishery ramps up before starting two way trip
all.surv <- seq(start.survey, end, by = freq.survey)
all.fish <- c(seq(start.fishery, start.fishery + 20, by = freq.fishery[1]),
              seq(start.fishery + 30, end, by = freq.fishery[2]))

# Information regarding sample intensity
high <- 100
low <- 20

# Get F from the package
tofind <- paste0("F[0-9]+-", my.spp)
ffiles <- sapply(tofind, grep, value = TRUE,
  x = list.files(system.file("cases", package = "ss3models"), full.names = TRUE))
done <- file.copy(ffiles, ".", recursive = TRUE, overwrite = TRUE)
if (!any(done)) {
    stop(paste("Fishing case files were not properly copied."))
}

# Years of survey index of abundance
done <- mapply(writeLines, con = paste0("index30-", my.spp, ".txt"),
  MoreArgs = list(text = c("fleets; 2", paste0("years; ",
  deparse(list(all.surv))), "sds_obs; list(0.2)")))

# Age comp
case_comp(fleets = 1:2,
  Nsamp = list(rep(high, length(all.fish)), rep(high, length(all.surv))),
  years = list(all.fish, all.surv), cpar = 2:1,
  type = "agecomp", case = 30, spp = my.spp)
# Length comp
case_comp(fleets = 1:2,
  Nsamp = list(rep(high, length(all.fish)), rep(high, length(all.surv))),
  years = list(all.fish, all.surv), cpar = 2:1,
  type = "lcomp", case = 30, spp = my.spp)

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
    paste("forecast_num;", my.forecasts[f])))
    )
}

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
    paste("forecast_num;", my.forecasts[f])))
    )
}

## Generate casefiles for different levels of age at 50% maturity
for(b in my.biology){
    counter <- which(my.biology == b) - 1
    file.current <- file(paste0("B", counter, "-cod.txt"), open = "w")
    writeLines(c(
    "# A case file to change the age at 50% maturity",
    "function_type; change_tv",
    "param; Mat50%_Fem",
    paste0("dev; rep(", b, ", 100)\n")), file.current)
    close(file.current)
  ## Generate casefiles for different lengths of forecasting using E
  for(f in 1:length(my.forecasts)){
    file.current <- file(paste0("E", f + counter * 20, "-cod.txt"), open = "w")
    writeLines(c(
    "# description: Fixed M, no qSurvey, w a given forecast number",
    "natM_type; 1Parm",
    "natM_n_breakpoints; NULL",
    "natM_lorenzen; NULL",
    "natM_val; c(NA, -1)",
    "par_name; Mat50%_Fem",
    paste("par_int;", ctl[ctl$Label == "Mat50%_Fem", "INIT"] + b),
    "par_phase; NA",
    paste("forecast_num;", my.forecasts[f])), file.current)
    close(file.current)
  }
}

setwd(wd.entry)