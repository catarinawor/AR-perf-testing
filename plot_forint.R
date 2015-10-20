#'Title: AR-perf-testing
#'
#'Author: Kelli Faye Johnson
#'
#'Contact: kellifayejohnson@gmail.com
#'
#'Date   : 2015-06-23

#+ intro, echo = FALSE
library(ggplot2)
library(gtable)
library(grid)
library(knitr)
library(xtable)

#+ data, echo = FALSE
if(substring(Sys.info()["user"], 1, 1) == "k") {
  if(exists("t:/AR-perf-testing")) {
    setwd("t:/AR-perf-testing/recruitdevs")
  } else setwd("c:/AR-perf-testing/recruitdevs")
}
file.data <- "EM.csv"
file.om   <- "OM.csv"
data.em <- read.csv(file.data)
data.om <- read.csv(file.om)

data <- merge(data.om, data.em, by = c("run", "replicate", "year"))

#+ ci, echo = FALSE
data[ c("SPB_low", "Rec_low")] <-
  data[, c("SPB.y", "Rec.y")] - 1.96 * data[, c("StdDev_SPB.y", "StdDev_Rec.y")]
data[ c("SPB_hi", "Rec_hi")] <-
  data[, c("SPB.y", "Rec.y")] + 1.96 * data[, c("StdDev_SPB.y", "StdDev_Rec.y")]
data$SPB_l   <- with(data, SPB.x < SPB_low)
data$SPB_h  <- with(data, SPB.x > SPB_hi)
data$SPB_out <- with(data, SPB.x < SPB_low | SPB.x > SPB_hi)
data$Rec_l   <- with(data, Rec.x < Rec_low)
data$Rec_h  <- with(data, Rec.x > Rec_hi)
data$Rec_out <- with(data, Rec.x < Rec_low | Rec.x > Rec_hi)

keepcols <- grep("_[a-z]{1}$|_out", colnames(data))
test <- aggregate(as.matrix(data[, keepcols]) ~ run + year, data = data, sum)
temp <- aggregate(as.matrix(data[, c("StdDev_SPB.y", "StdDev_Rec.y")]) ~ run + year,
  data = data, mean)

test <- merge(test, temp, by = c("run", "year"))


mf_labeller <- function(var, value){
    value <- as.character(value)
    if (var == "run") {
        value[value=="cay"] <- as.character(-0.25)
        value[value=="cby"] <- as.character(0.00)
        value[value=="ccy"] <- as.character(0.25)
        value[value=="cdy"] <- as.character(0.50)
        value[value=="cey"] <- as.character(0.75)
        value[value=="cfy"] <- as.character(0.90)
    }
    return(value)
}

#' The first 25 years acted as a burn in period, thus prediction intervals are
#' not expected to include the output from the operating model on a consistent
#' basis. Thus the following plots are limited to years 25 through 100.
#'
#' Forecasts start in year 80 and the level of the autocorrelation increases as
#' one moves down the row. Negative autocorrelation means that closer values are
#' less correlated, where higher values more correlated or similar.
#'
#+ plots, echo = FALSE, warning = FALSE
my.cols <- as.vector(t(outer(c("SPB_", "Rec_"), c("out", "l", "h"), paste0)))
my.maxy <- max(test[, c("SPB_l", "SPB_h", "SPB_out", "Rec_l", "Rec_h", "Rec_out")])

counter <- 0
for (cols in my.cols) {
    counter <- counter + 1
    y2 <- ifelse(grepl("SPB", cols), "StdDev_SPB.y", "StdDev_Rec.y")
    if (grepl("out", cols)) beg <- "Outside"
    if (grepl("l", cols))   beg <- "Lower"
    if (grepl("h", cols))   beg <- "Higher"
    if (grepl("SPB", cols)) end <- "SSB"
    if (grepl("Rec", cols)) end <- "recruitment"
    ylab <- paste(beg, ifelse(beg == "Outside", "the", "than the"), "95%", end, "interval")
 if (counter %in% c(1, 4)){
     p <- ggplot(data, aes(year, eval(parse(text = y2)))) +
     geom_point(aes(color = run), alpha = 0.2) + theme_bw() +
     coord_cartesian(xlim = c(25, 100)) +
     geom_vline(xintercept = 80, color = "red") +
     ylab(paste("Standard deviation of", end, "from EM"))
     print(p)
 }
  p <- ggplot(test, aes(year, eval(parse(text = cols)))) +
    geom_point() + theme_bw() +
    coord_cartesian(xlim = c(25, 100), ylim = c(0, my.maxy)) +
    facet_grid(run ~ ., scales = "fixed", labeller = mf_labeller) +
    geom_vline(xintercept = 80, color = "red") +
    ylab(ylab)
  print(p)
}

