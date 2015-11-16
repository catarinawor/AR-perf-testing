
# MLE function
NLL = function( parvec, recdev, returntype="NLL" ){
  sigmar = exp(parvec[1])
  AR = plogis(parvec[2])*2 - 1
  # Actual likelihood
  NLL = 0
  for(t in 1:length(recdev)){
    if(t==1) NLL = NLL - dnorm(recdev[t], mean=0, sd=sigmar, log=TRUE)
    if(t>=2) NLL = NLL - dnorm(recdev[t], mean=AR*recdev[t-1], sd=sigmar*sqrt(1-AR^2), log=TRUE)
  }
  # Penalized likelihood from SS
  PNLL = length(recdev)*log(sigmar)
  square = function(a) a^2
  two_sigmaRsq = 2*sigmar^2
  for(t in 1:length(recdev)){
    if(t==1) PNLL = PNLL + square(recdev[t])/two_sigmaRsq
    if(t>=2) PNLL = PNLL + square(recdev[t]-AR*recdev[t-1]) / ((1.0-AR*AR)*two_sigmaRsq)
  }
  # Penalized likelihood from SS -- ATTEMPTS TO FIX IT! (because it should yield identical estimates to NLL
  PNLL_V2 = 0
  square = function(a) a^2
  two_sigmaRsq = 2*sigmar^2
  for(t in 1:length(recdev)){
    if(t==1) PNLL_V2 = PNLL_V2 - ( -log(sigmar) - square(recdev[t])/two_sigmaRsq )
    if(t>=2) PNLL_V2 = PNLL_V2 - ( -log(sigmar*sqrt(1-AR^2)) - square(recdev[t]-AR*recdev[t-1]) / ((1.0-AR*AR)*two_sigmaRsq) )  # + square(recdev[t]-AR*recdev[t-1]) / ((1.0-AR*AR)*two_sigmaRsq)
  }
  # Return stuff
  if(returntype=="NLL") Return = NLL
  if(returntype=="PNLL") Return = PNLL
  if(returntype=="PNLL_V2") Return = PNLL_V2
  if(returntype=="Params") Return = c("sigmar"=sigmar, "AR"=AR)
  return( Return )
}

library(devtools)
#install_github("james-thorson/utilities")
library(ThorsonUtilities)

Par = scan_admb_par( "testing fix/ss3.par" )

# Internal
SigmaR = Par['# SR_parm[3]:']
AR = Par['# SR_parm[6]:']
c(SigmaR, AR)

# External
RecDev = Par[grep("recdev",names(Par))]
sd( RecDev )
sqrt(mean(RecDev*RecDev))
acf( RecDev )[1]
mean(RecDev[-1]*RecDev[-length(RecDev)])/mean(RecDev*RecDev)
# Calculate MLE
Opt = nlminb( start=c(0,0), recdev=RecDev, objective=NLL, returntype="NLL")
NLL( Opt$par, recdev=RecDev, returntype="Params")
# Calculate MLE
Opt = nlminb( start=c(0,0), recdev=RecDev, objective=NLL, returntype="PNLL")
NLL( Opt$par, recdev=RecDev, returntype="Params")
# Calculate MPLE
Opt = nlminb( start=c(0,0), recdev=RecDev, objective=NLL, returntype="PNLL_V2")
NLL( Opt$par, recdev=RecDev, returntype="Params")
