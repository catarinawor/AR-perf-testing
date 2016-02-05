# Can autocorrelated recruitment be estimated using integrated assessment models and how does it affect population forecasts?
This repository houses simulation code, Stock Synthesis (SS) executables, 
and a resulting manuscript which was submitted to Fisheries Research,
entitled: "Can autocorrelated recruitment be estimated using integrated 
assessment models and how does it affect population forecasts?". 

This paper outlines a method to account for autocorrelation in estimated 
recruitment deviations when using age-structured stock assessment models. 
Autocorrelated recruitment can be caused by several factors, 
but are typically attributed to multi-year environmental drivers 
affecting early life survival rates. 
We found that estimating autocorrelation from estimates of 
recruitment residuals as output from an integrated assessment model 
resulted in less a biased estimate than when autocorrelation was 
estimated internally within the model.

Understanding variability in fishes is one of the greatest challenges 
faced by fisheries scientists today. 
Accounting for autocorrelation in recruitment can lead to stock projections which are 
different than when autocorrelation is not accounted for and 
thus may be integral to rebuilding plans and the recovery of some stocks. 
This manuscript is the first to assess the performance of 
stock assessment forecasts when recruitment is autocorrelated. 
Methods that work towards decreasing known biases in stock assessment models 
are important for increasing the 
sustainability of marine resources and likely to be highly cited. 

This  work contributed towards  the  work  plan  of  the  
ICES  Working  Group  on  Recruitment  Forecasting  in  a  variable  Environment  (WGRFE).
If you have any questions please feel free to contact Kelli Faye Johnson,
the corresponding author, at kfjohns@uw.edu

## Simulation
### Instructions for installing the correct version of ss3sim
  devtools::install.github("r4ss/r4ss@master")
  devtools::install.github("ss3sim/ss3sim@master")
  devtools::install.github("ss3sim/ss3models@master")
### Running the simulation
To run the simulation start with [AR_Simulations](AR-perf-testing/AR_Simulations.r),
which will install of the necessary packages and run each script.

## Word documents
The [manuscript](AR-perf-testing/paper_02_05_2016.docx)
was submitted on February 05, 2016. 
A final version of the paper will be available, if it is accepted.

