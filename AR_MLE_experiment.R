
# MLE function
NLL = function( parvec, recdev, returntype="NLL" ){
  sigmar = exp(parvec[1])
  AR = plogis(parvec[2])*2 - 1
  NLL = 0
  for(t in 1:length(recdev)){
    if(t==1) NLL = NLL - dnorm(recdev[t], mean=0, sd=sigmar, log=TRUE)
    if(t>=2) NLL = NLL - dnorm(recdev[t], mean=AR*recdev[t-1], sd=sigmar*sqrt(1-AR^2), log=TRUE)
  }
  if(returntype=="NLL") Return = NLL
  if(returntype=="Params") Return = c("sigmar"=sigmar, "AR"=AR)
  return( Return )
}

# Settings
SigmaR = 0.5
AR_set = c(-0.25, 0, 0.25, 0.5, 0.75, 0.9)
n_years = 100
n_reps = 50

# Generate rec-devs
RecDevs = array(NA, dim=c(length(AR_set),n_reps,n_years))
for(s in 1:length(AR_set)){
for(r in 1:n_reps){
for(t in 1:n_years){
  if(t==1) RecDevs[s,r,t] = rnorm(1, mean=0, sd=SigmaR)
  if(t>=2) RecDevs[s,r,t] = rnorm(1, mean=AR_set[s]*RecDevs[s,r,t-1], sd=SigmaR*sqrt(1-AR_set[s]^2))
}}}

# Compare MLE vs. estimator used in "external" method
Results = array(NA,dim=c(length(AR_set),n_reps,3),dimnames=list(NULL,NULL,c("True","MLE","external")))
for(s in 1:length(AR_set)){
for(r in 1:n_reps){
  Results[s,r,'True'] = AR_set[s]
  Results[s,r,'external'] = acf( RecDevs[s,r,] )$acf[2]
  # Calculate MLE
  Opt = nlminb( start=c(0,0), recdev=RecDevs[s,r,], objective=NLL)
  ParHat = NLL( Opt$par, recdev=RecDevs[s,r,], returntype="Params")
  Results[s,r,'MLE'] = ParHat['AR']
}}

# Plot results
png( file="Experiment_comparing_MLE_and_external_estimators.png", width=10, height=2, res=200, units="in")
  par( mfrow=c(1,length(AR_set)), mar=c(1,1,2,0), mgp=c(1.75,0.25,0), tck=-0.02, oma=c(2,2,0,0))
  for(s in 1:length(AR_set)){
    plot( x=Results[s,,'external'], y=Results[s,,'MLE'], main=paste0("AR=",AR_set[s]), xlim=c(-1,1), ylim=c(-1,1), xlab="", ylab="")
    abline( a=0, b=1)
  }
  mtext(side=1, outer=TRUE, text="using acf function", line=0.5)
  mtext(side=2, outer=TRUE, text="MLE", line=0.5)
dev.off()
