
# Library and directory
library( r4ss )
RootFile = "C:/Users/James.Thorson/Desktop/Project_git/AR-perf-testing/canary_2015_base/"

# Internal
Run0 = paste0(RootFile,"base + new EXE + logistic maturity")
Run1 = paste0(RootFile,"base + new EXE + logistic maturity + external AR")
Run2 = paste0(RootFile,"base + new EXE + logistic maturity + internal AR")

################
# Estimate AR internally
################

# Plots
Covar = Forecast = TRUE
  SsOutput = SS_output(Run1, covar=Covar, forecast=Forecast, printstats=TRUE, ncols=210)
  # All
  Plots = SS_plots(SsOutput, png=TRUE, printfolder="Plots_Default", uncertainty=Covar, aalresids=TRUE, datplot=TRUE, areanames=c("CA","OR","WA"), multifig_colpolygon=c( rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0,1,0,0.5)), multifig_oma=c(5,5,1,2)+.1, linescol=c("green", "red", "blue"), andre_oma=c(3,0,0.5,0) ) #aalyear, aalbin

# Estimate external rec-dev AR
RecDev = SsOutput$recruitpars[,c("Num","Label","Value")]
  RecDev_main = RecDev[grep("Main_",RecDev$Label),]
  AR = acf( RecDev_main$Value )
  AR['1']

################
# Estimate AR internally
################

Output0 = SS_output(Run0, covar=TRUE, forecast=TRUE, printstats=TRUE, ncols=210)
Output1 = SS_output(Run1, covar=TRUE, forecast=TRUE, printstats=TRUE, ncols=210)
Output2 = SS_output(Run2, covar=TRUE, forecast=TRUE, printstats=TRUE, ncols=210)

# Plot 1960-2026
Which = match( paste0("Bratio_",1960:2026), Output0$derived_quants$LABEL )                                                                                                                    
Output0$derived_quants[Which,]

par( mfrow=c(1,3), mar=c(3,3,2,0), mgp=c(1.5,0.25,0), tck=-0.02)
  SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn( x=1960:2026, y=Output0$derived_quants[Which,'Value'], fn=plot, type="l", ylim=c(0.0,0.7), ybounds=Output0$derived_quants[Which,'Value']%o%c(1,1)+Output0$derived_quants[Which,'StdDev']%o%c(-1,1), col_bounds=rgb(1,0,0,0.2), bounds_type="shading")
  lines( x=1960:2026, y=Output0$derived_quants[Which,'StdDev']/Output0$derived_quants[Which,'Value'], col="blue", lwd=3)
  SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn( x=1960:2026, y=Output1$derived_quants[Which,'Value'], fn=plot, type="l", ylim=c(0.0,0.7), ybounds=Output1$derived_quants[Which,'Value']%o%c(1,1)+Output1$derived_quants[Which,'StdDev']%o%c(-1,1), col_bounds=rgb(1,0,0,0.2), bounds_type="shading")
  lines( x=1960:2026, y=Output1$derived_quants[Which,'StdDev']/Output1$derived_quants[Which,'Value'], col="blue", lwd=3)
  SpatialDeltaGLMM::Plot_Points_and_Bounds_Fn( x=1960:2026, y=Output2$derived_quants[Which,'Value'], fn=plot, type="l", ylim=c(0.0,0.7), ybounds=Output2$derived_quants[Which,'Value']%o%c(1,1)+Output2$derived_quants[Which,'StdDev']%o%c(-1,1), col_bounds=rgb(1,0,0,0.2), bounds_type="shading")
  lines( x=1960:2026, y=Output2$derived_quants[Which,'StdDev']/Output2$derived_quants[Which,'Value'], col="blue", lwd=3)
    