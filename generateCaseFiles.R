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
neededobjects <- c("my.forecasts", "my.biology")
sapply(neededobjects, function(x) {
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
wd.casefiles <- system.file("extdata/eg-cases", package = "ss3sim")

## Generate casefiles for different lengths of forecasting using E
for(f in 1:length(my.forecasts)){
    file.current <- file(paste0(wd.casefiles, "/E", f, "-cod.txt"), 
                         open = "w")
    writeLines(c(
    "# description: Fixed M, no qSurvey, w a given forecast number",
    "natM_type; 1Parm",
    "natM_n_breakpoints; NULL",
    "natM_lorenzen; NULL",
    "natM_val; c(NA, 1)",
    "par_name; LnQ_base_3_CPUE",
    "par_int; NA",
    "par_phase; -1",
    paste("forecast_num;", my.forecasts[f])), file.current)
    close(file.current)
}

## Generate casefiles for different levels of age at 50% maturity
for(b in my.biology){
    counter <- which(my.biology == b) - 1
    file.current <- file(paste0(wd.casefiles, "/B", counter, "-cod.txt"), 
                         open = "w")
    writeLines(c(
    "# A case file to change the age at 50% maturity",
    "function_type; change_tv",
    "param; Mat50%_Fem",
    paste0("dev; rep(", b, ", 100)\n")), file.current)
    close(file.current)
}