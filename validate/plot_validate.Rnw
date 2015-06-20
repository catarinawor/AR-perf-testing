\documentclass{article}
\usepackage[T1]{fontenc}

\begin{document}
Scenarios (\Sexpr{ss3sim::id_scenarios(getwd())}) with
\Sexpr{max(dir(ss3sim::id_scenarios(getwd())[1], pattern = "[0-9]$"))} iterations,
where there are age- and length-compositions per sampling year
for both the fishery and the survey.

<<setup, include=FALSE>>=
options(xtable.comment = FALSE)
@

<<echo = FALSE>>=
library(ss3sim)
library(xtable)
scalars <- read.csv("ss3sim_scalar.csv", header = TRUE)
ts <- read.csv("ss3sim_ts.csv", header = TRUE)
scalars <- calculate_re(scalars, TRUE)

ts$SSB_re <- with(ts, ((SpawnBio_om - SpawnBio_em) / SpawnBio_om))
ts$R0_re <- with(ts, ((Recruit_0_om - Recruit_0_em) / Recruit_0_om))
ts$F_re <- with(ts, ((F_om - F_em) / F_om))
@

<<echo = FALSE, fig.height = 8>>=
numscen <- length(unique(ts$scenario))
scenarios <- as.character(unique(ts$scenario))
for (scen in 1:numscen) {
par(mfrow = c(3, 1), mar = c(0, 5, 0, 1), oma = c(7, 1, 2, 2),
  tck = -0.001, mgp = c(2.75, 0.15, 0), las = 1)
for (par in c("SSB_re", "R0_re", "F_re")) {
 # Time series boxplot
x <- aggregate(eval(parse(text = par)) ~ year,
  data = ts[ts$scenario == scenarios[scen], ], mean)
boxplot(eval(parse(text = par)) ~ year,
    data = ts[ts$scenario == scenarios[scen], ], ylab = par,
  ylim = c(-1, 1), xaxt = "n")
getdatanum <- ss3sim:::get_caseval(scenarios[scen], case = "D")
getlnthnum <- ss3sim:::get_caseval(scenarios[scen], case = "L")
abline(h = 0, lty = 2, col = "red")
  if (par == "SSB_re") {
    legend("topright", legend = "legend reveals years with data", bty = "n")
    legend("bottomright",
      legend = c("Lognormal", "Multinomial", "Dirichlet"), pch = c(3, 19, 24),
      bty = "n")
    x.mast <- ss3sim:::get_args(file.path("..", "cases",
      paste0("index", getdatanum,"-cod.txt")))
    l.mast <- ss3sim:::get_args(file.path("..", "cases",
      paste0("lcomp", getlnthnum, "-cod.txt")))

    height <- 0.8
    y <- par("yaxp")[2]
    x <- x.mast$years[[1]]
    points(x, rep(y * height, length(x)), pch = 3)
    text(-4, par("yaxp")[2] * height,
      paste0("survey (CV=", x.mast$sds_obs[[1]], ")"), pos = 4)

    for (comps in seq_along(l.mast$fleets)){
    height <- height - 0.15
    type <- ifelse(l.mast$cpar[[comps]] == 1, "fish", "surv")
      x <- l.mast$years[[comps]]
      points(x, rep(y * height, length(x)),
        pch = ifelse(l.mast$cpar[comps] == 1, 19, 24))
      text(-4, par("yaxp")[2] * height,
        paste0(type, " comp (n=", l.mast$Nsamp[[comps]][1], ")"), pos = 4)
    }
  }
}
  par(new = TRUE)
  plot(1:100, rep(1, 100), ylab = "", xlab = "", yaxt = "n", type = "n")
  axis(1)
  mtext(side = 1, outer = TRUE, "years", line = 2)
  legend("bottomright", legend = "*scales (-1:1) are symmetrical.",
    bty = "n")
  mtext(side = 1, outer = TRUE, line = 4,
    paste0("Figure", scen, ". Relative error for ", scenarios[scen],
      ifelse(length(dir(scenarios[scen], pattern = "bias")) == 0, " without", " with"),
      " bias adjustment."))
}
@

<<echo = FALSE, results = 'asis'>>=
for (scen in 1:numscen){
temp <- list()
counter <- 1
store <- vector()
mynames <- grep("_re", names(scalars), value = FALSE)
for(ind in mynames) {
  if (!all(is.numeric(scalars[scalars$scenario == scenarios[scen], ind]))) next
  if (all(is.na(scalars[scalars$scenario == scenarios[scen], ind]))) next
  if (all(scalars[scalars$scenario == scenarios[scen], ind] == 0)) next
  temp[[counter]] <- aggregate(as.matrix(scalars[scalars$scenario == scenarios[scen], ind]) ~ species,
    data = scalars[scalars$scenario == scenarios[scen], ],
    mean, na.rm = TRUE)
  if (grepl("LnQ", colnames(scalars)[ind])) {
    temp[[counter]] <- aggregate((
      exp(scalars[scalars$scenario == scenarios[scen], "LnQ_base_2_Survey_om"]) -
      exp(scalars[scalars$scenario == scenarios[scen], "LnQ_base_2_Survey_em"])) /
      exp(scalars[scalars$scenario == scenarios[scen], "LnQ_base_2_Survey_om"]) ~
      species,
      data = scalars[scalars$scenario == scenarios[scen], ], mean,
      na.rm = TRUE)
  }
  counter <- counter + 1
  store <- append(store, ind, length(store) + 1)
}
temp <- do.call("rbind", sapply(temp, "[", 2))
rownames(temp) <- colnames(scalars)[store]
print(xtable(temp, digits = 5,
  caption = paste0("Relative error in estimated and derived parameters, for",
    scenarios[scen], ". Note q is exp(ln(q)).")), include.rownames = TRUE,
  include.colnames = FALSE,
  )
}
@

\end{document}
