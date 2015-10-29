
##################
# TO USE:
# 1. Run "AR_Simulations.r" with at least 1 simulation replicate
# 2. Then run script below
##################

library(r4ss)

RootFile = "C:/Users/James.Thorson/Desktop/Project_git/AR-perf-testing/"
 # no label -- estimate internally
 # t -- fixed at true value
 # x -- estimate externally and then fix
 # z -- fix at zero

# Panel plot
png( file=paste0(RootFile,"Example_SPB.png"), width=9, height=6, res=200, units="in")
  RepI = 1
  par( mfcol=c(4,6), mar=c(0,0,0,0), mgp=c(2,0.5,0), tck=-0.02, oma=c(3.5,3.5,3.5,3.5), yaxs="i")
  for(i in 1:6){
  for(j in 1:4){
    # Directory
    Code = paste0( "A100-D0-E20-F1-L100-c", letters[i], c("yt","yz","y","yx")[j] )
    RunFile = paste0( RootFile,Code,"/")

    # Load results
    Output = SS_output(paste0(RunFile,RepI,"/em/"), covar=TRUE, forecast=TRUE, printstats=TRUE, ncols=210)
    Deriv = Output$derived_quants
    Deriv = Deriv[match(paste0("SPB_",1:100),Deriv$LABEL),]
    Results = read.csv( paste0(RunFile,"results_ts_",Code,".csv") )
    Results = Results[which(Results$replicate==1),]

    # Plot
    Ybounds = Deriv[,'Value']%o%c(1,1)+Deriv[,'StdDev']%o%c(-1.96,1.96)
    SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn( fn=plot, x=1:100, y=Deriv[,'Value'], xaxt="n", yaxt="n", ylab="", xlab="", type="l", ylim=c(0,1.05*max(Ybounds,Results[,'SpawnBio_om'])), ybounds=Ybounds, col_bounds=rgb(1,0,0,0.2), bounds_type="shading", col="red", lwd=1.5)
    lines( x=1:100, y=Results[,'SpawnBio_om'], lwd=2)

    # Labeling
    trunc_last = function(x) rev( rev(x)[-1])
    if(j==4) axis(1, at=trunc_last(pretty(axTicks(1))), labels=trunc_last(pretty(axTicks(1))) )
    if(i==1) axis(2)
    if(j==1) mtext( side=3, text=c(-0.25, 0, 0.25, 0.5, 0.75, 0.9)[i] )
    if(i==6) mtext( side=4, text=c("True", "Zero", "Internal", "External")[j], line=0.5 )

  }}
  # Outer labels
  mtext( side=1, text="Year", outer=TRUE, line=2)
  mtext( side=2, text="Spawning output", outer=TRUE, line=2)
  mtext( side=3, text="True autocorrelation", outer=TRUE, line=2)
  mtext( side=4, text="Estimation model", outer=TRUE, line=2)
dev.off()
