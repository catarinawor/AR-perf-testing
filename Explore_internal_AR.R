
################################
# Problems
# 1. Missing projection of current AR into future AR

library(ThorsonUtilities)

SourceFile = "C:/Users/James.Thorson/Desktop/Project_git/AR-perf-testing/A915000-D91-E920-F91-L915000-cey/"
RepNum = 2

# True values
Par_true = scan_admb_par( paste0(SourceFile,RepNum,"/om/ss3_24~2.par") )
RecDev_true = Par_true[grep("recdev",names(Par_true))][-c(1:25)]
ar( RecDev_true )
sd( RecDev_true )
# Estimated values
Par_hat = scan_admb_par( paste0(SourceFile,RepNum,"/em/ss3_24~1.par") )
RecDev_hat = Par_hat[grep("recdev",names(Par_hat))][-c(1:25)]
ar( RecDev_hat )
sd( RecDev_hat )
Par_hat[match("# SR_parm[6]:",names(Par_hat))]
