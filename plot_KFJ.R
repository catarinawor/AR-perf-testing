###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate : 2015-02-19
####LastModBy  : Eliza councill
####Purpose    : Plot results for AR Performance testing
####Packages   : cumplyr, reshape, reshape2, plry, ss3sim
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################


###############################################################################
###############################################################################
## Step 01
## Set up the variable inputs
###############################################################################
###############################################################################
axis.col <- 1 #gray(.3)
box.col  <- 1 #gray(.3)
alpha.levels <- c(0.1, 0.5)

###############################################################################
###############################################################################
## Step 02
## install the packages
###############################################################################
###############################################################################
library(cumplyr)
library(plyr)
library(reshape)
library(reshape2)
library(ss3sim)

###############################################################################
###############################################################################
## Step 03
## Read in the data
###############################################################################
###############################################################################
setwd("C:/Users/Elizabeth.Councill/Documents/copies")
scalars <- read.csv(dir(pattern = "_scalar.csv"), stringsAsFactors = FALSE)
ts      <- read.csv(dir(pattern = "_ts.csv"), stringsAsFactors = FALSE)

###############################################################################
###############################################################################
#### Step 04
#### Manipulate the data files
###############################################################################
###############################################################################
scalars <- calculate_re(scalars)
ts <- calculate_re(ts)
ts <- merge(x = ts,
  y = subset(scalars, select = c("ID", "hessian", "params_on_bound_em" )),
  by.x = "ID", by.y = "ID", all = TRUE)

###############################################################################
###############################################################################
## Step 05
## plotting functions
###############################################################################
###############################################################################
add.label <- function(label, ...) {
  legend("topleft", legend = " ", title = label, bty = "n", ...)
}

add.polygon <- function(data = ts,  y, xvals,
                      alpha.level, alpha.min = 0, alpha.max = 1){
  ## Pass this a series of years (x) and a matrix of trajectories (df) and it
  ## adds a polygon to the current plot with whose area contains 1-alpha.level
  ## Depends on reshape for the cast function
  data <- data[order(data$year), ]
  df.wide <- cast(data = data, formula = replicate ~ year, value = y)[,-1]
  x <- as.numeric(names(df.wide))
  alpha.level <- sort(alpha.level)
  for(i in 1:length(alpha.level)){
    alpha <- alpha.level[i]
    alpha.col <- 1 - (alpha.min + alpha * (alpha.max - alpha.min))
    col.poly <- rgb(alpha.col, alpha.col, alpha.col, alpha = 1)
    quantiles.temp <-  as.matrix(t(apply(df.wide, 2, quantile,
      probs = c(alpha / 2, 1 - alpha / 2), name = FALSE, na.rm = TRUE)))
    polygon(x = c(x, rev(x)), y = c(quantiles.temp[, 1],
      rev(quantiles.temp[, 2])), col = col.poly, border = NA)
  }
  return(invisible(df.wide))
}

# Plot time series data with polygons for alpha levels
f.ts<- function(y = "SpawnBio_re", data = ts, Es, spp = "cod",
  xlim = range(ts$year), ...) {
  years <- xlim[1]:xlim[2]
  ts.temp <- subset(ts, E %in% Es & species %in% spp & year %in% years)
  plot(x = years, y = years, xlim = xlim, ylim = c(-1.2, 1.2),
    ann = FALSE, axes = FALSE)
  if(dim(ts.temp)[1] > 0) {
    add.polygon(data = ts.temp, y = y, xvals = years,
      alpha.level = alpha.levels, alpha.min = 0.15)
  medians <- tapply(ts.temp[, y], ts.temp$year, median)
  lines(x = years[which(years %in% as.numeric(names(medians)))],
    y = as.vector(medians), lty = 1, lwd = 2)
  abline(h = 0, col = 1, lty = 2, lwd = 0.75)
  }
}

###############################################################################
###############################################################################
#### Step 06
#### Plot relative error in predictions of spawning stock biomass
#### Vertical solid line indicates year in which predictions begin, with
#### increasing number of predictions going down the rows.
#### Could add columns for levels of autocorrelation or bias corrected or not.
###############################################################################
###############################################################################
cases.E <- paste0("E", 1:6)
my.forecasts <- c(0, 1, 3, 5, 10, 20)
## Add increasing columns by changing second value of mfrow argument and adding
## a for loop, inside the current loop because R will plot across columns first
par(mfrow = c(6, 1), mgp = c(1, 0.5, 0), cex.axis = 1, tck = -0.01,
    mar = rep(0.25, 4), oma = c(2, 3, 1.5, 0), col.axis = axis.col)
for(e in cases.E) {
    f.ts(y = "SpawnBio_re", Es = e, spp = "anb", xlim = c(50, 100))
    abline(v = max(ts$year) - my.forecasts[which(cases.E == e)])
    if(e == tail(cases.E, n = 1)) axis(1, outer = FALSE)
}
  mtext(expression(paste("Relative error: ", italic(SSB))), side = 2,
        line = -0.5, outer = TRUE)

###############################################################################
###############################################################################
#### Step 07
#### Plot relative error in parameters across autocorrelation
###############################################################################
###############################################################################
# Change the number after E to change the number of forecast years
cases.E <- "E1"
# Use this line to see what parameters you can plot
grep("_re", names(scalars), value = TRUE)
whichpar <- "CV_old_Fem_GP_1_re" #"SR_sigmaR_re"

cases.spp <- unique(sapply(strsplit(unique(scalars$species), ""), "[", 1))
ylim <- max(abs(range(scalars[, grep(whichpar, names(scalars))])))
ylim <- c(-1 * ylim, ylim)

for(spp in cases.spp){
  xx <- scalars
  xx <- droplevels(subset(xx, E == cases.E,
    select = c(eval(parse(text = whichpar)), E, species, bias.tried)))
  xx <- xx[order(xx$species), ]

  par(mfrow = c(1, 2), mgp = c(1, 0.5, 0), cex.axis = 1, tck = -0.01,
      mar = c(3, 0.5, 0.5, 0.5), oma = c(2, 5, 1.5, 0), col.axis = axis.col)
  my.line <- 0.25
  with(xx[is.na(xx$bias.tried), ],
    boxplot(eval(parse(text = whichpar)) ~ species, names = NA,
    las = 1, border = 1, horizontal = TRUE, ylim = ylim, varwidth = TRUE))
  axis(2, at = 1:length(cases.spp), labels = cases.spp,
    las = 1, col.axis = 1, cex.axis = 1.25)
  mtext("No bias adjustment", line = my.line)
  abline(v = 0, lty = 2, col = 1, lwd = 2)
  box(col = axis.col)

  with(xx[xx$bias.tried > 0, ],
    boxplot(eval(parse(text = whichpar)) ~ species, names = NA,
    las = 1, border = 1, horizontal = TRUE, ylim = ylim, varwidth = TRUE))
  mtext("Bias adjustment", line = my.line)
  abline(v = 0, lty = 2, col = 1, lwd = 2)
  mtext(side = 1, line = -0.5, outer = TRUE,
    text = paste("Relative error: ", gsub("_re", "", whichpar)))
  box(col = axis.col)
}
