
#' Read in the results
getresults <- FALSE
if (getresults) {
  get_results_all(overwrite_files = TRUE, parallel = doparallel)
}
sc <- read.csv("ss3sim_scalar.csv")
ts <- read.csv("ss3sim_ts.csv")
dq <- read.csv("ss3sim_dq.csv")
sc$bias <- ifelse(sc$max_bias_adj_em == -1, "no", "yes")

#' Calculate relative error
sc <- calculate_re(sc, add = TRUE)
ts <- calculate_re(ts, add = TRUE)

#' Add max grad and par on bounds
get <- c("max_grad", "params_on_bound_em", "bias", "SR_sigmaR_om")
ts <- data.frame(ts, sc[match(ts$ID, sc$ID), get])
dq <- data.frame(dq, sc[match(dq$ID, sc$ID), get])

#' Calculate cis
ci <- calulate_ci(dq, verbose = TRUE)
ci25 <- calulate_ci(dq, coverage = .95, verbose = TRUE)
ci <- data.frame(ci, sc[match(ci$ID, sc$ID), get])
ci25 <- data.frame(ci25, sc[match(ci$ID, sc$ID), get])

#' Add names
sc$AR <- factAR(sc$species)
sc$EM <- factEM(sc$species)
ts$AR <- factAR(ts$species)
ts$EM <- factEM(ts$species)
dq$AR <- factAR(dq$species)
dq$EM <- factEM(dq$species)
ci$AR <- factAR(ci$species)
ci$EM <- factEM(ci$species)
ci25$AR <- factAR(ci25$species)
ci25$EM <- factEM(ci25$species)

#' Subset
#' Keep raw data
raw.sc <- sc
raw.ts <- ts
raw.dq <- dq
raw.ci <- ci
raw.25 <- ci25
#' subset
sc <- subset(sc, max_grad < 0.01 & params_on_bound_em < 1 & bias == "yes")
ts <- subset(ts, max_grad < 0.01 & params_on_bound_em < 1 & bias == "yes")
dq <- subset(dq, max_grad < 0.01 & params_on_bound_em < 1 & bias == "yes")
ci <- subset(ci, max_grad < 0.01 & params_on_bound_em < 1 & bias == "yes")
ci25 <- subset(ci25, max_grad < 0.01 & params_on_bound_em < 1 & bias == "yes")
keepem <- c("Internal", "External")
allem <- c("Internal", "True", "Zero", "External")

#' Set the theme
theme <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.key = element_rect(colour = "white"),
        legend.title = element_text(size = 0, colour = "white")
  )

###############################################################################
###############################################################################
#### Figure exampleAR
###############################################################################
###############################################################################
sub <- c(1, 2, 5)
linewd <- 1.8
toplot <- sapply(EpsList, function(x) x[, 1])[, sub]
jpeg(file.path(fig_folder, "exampleAR.jpeg"), quality = 100,
  width = width, height = height/2)
   matplot(toplot, type = "l", las = 1, lty = c(2, 1, 3), col = "black",
     xlab = "year", ylab = "recruitment deviations", lwd = linewd,
     xlim = c(0, 50))
   legend("topleft", legend = as.character(format(AR[sub], digits = 2)),
     lty = c(2, 1, 3), bty = "n", lwd = linewd, horiz = TRUE)
dev.off()

###############################################################################
###############################################################################
#### Figure estimatedAR
###############################################################################
###############################################################################
#' Plot AR internal versus zero_ext for the maximum amount of data
x <- "EM"; y <- "SR_autocorr_em"; z <- "AR"
xlab <- "Estimated autocorrelation"
form <- ss3sim:::facet_form(x, NULL, z, NULL)
temp <- droplevels(subset(sc,
    A == "A101" & EM %in% keepem & SR_sigmaR_om == SDmarg))

rline <- data.frame("EM" = rep(keepem, each = 6),
                    "AR" = rep(unique(sc$AR), length(keepem)),
                    rep(unique(sc[sc$EM == "True", y]), length(keepem)))
colnames(rline)[3] <- y

g <- ggplot(temp) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = y), linetype = "dashed",
    data = rline, col = "red") +
  xlab(expression(rho)) +
  theme + theme(legend.position = c(0.08, 0.8)) +
  geom_text(data = aggregate(replicate ~ AR + EM, data = temp,
    function(x) length(unique(x))), aes(label = replicate,
    x = -1, y = 100), hjust = "inward")
ggsave(file.path(fig_folder, "estimatedAR.png"), g, height = 3.5, width = 8)

###############################################################################
###############################################################################
#### Figure estimatedlnr0
###############################################################################
###############################################################################
x <- "EM"; y <- "SR_LN_R0_em"; z <- "AR"
xlab <- expression(ln(r[0]))
form <- ss3sim:::facet_form(x, NULL, z, NULL)
temp <- droplevels(subset(sc,
    A == "A101" & EM %in% keepem & SR_sigmaR_om == SDmarg))

g <- ggplot(temp) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = unique(sc$SR_LN_R0_om)),
    linetype = "dashed", col = "red") +
  xlab(xlab) +
  theme + theme(legend.position = c(0.08, 0.8)) +
  geom_text(data = aggregate(replicate ~ AR + EM, data = temp,
    function(x) length(unique(x))), aes(label = replicate,
    x = -Inf, y = 100), hjust = "inward")
ggsave(file.path(fig_folder, "estimatedlnr0.png"), g, height = 3.5, width = 8)

###############################################################################
###############################################################################
#### Figure tsSSB
###############################################################################
###############################################################################
#' Plot Spawning Stock Biomass internal versus external
#' for the maximum amount of data
x <- "year"; y <- "SpawnBio_re"; z <- "AR"; zz <- "EM"
xlab <- "year"; ylab <- "Relative error in spawning biomass"
form <- ss3sim:::facet_form(zz, NULL, z, NULL)
temp <- droplevels(subset(ts,
  A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))
temp$time <- ifelse(temp$year > 80, "forecast", "fishing")
temp$time[temp$year < 26] <- "burnin"

lims <- list(x = c(25, 100), y = c(-0.5, 0.75))

g <- ggplot(data = temp, aes_string(x = x)) +
  geom_boxplot(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  geom_vline(aes_string(xintercept = 80), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(lims$x) + ylim(lims$y) +
  theme + theme(legend.position = c(0.08, 0.8)) +
  # Calculate the number of converged replicates
  geom_text(data = aggregate(replicate ~ A + AR + EM, data = temp,
    function(x) length(unique(x))), aes(label = replicate,
    x = lims$x[1], y = lims$y[2]), hjust = "inward") +
  # Calculate the Mean Absolute relative error during forecast
  geom_text(data = aggregate(SpawnBio_re ~ A + AR + EM + time,
    data = subset(temp, time == "forecast"),
    function(x) round(mean(abs(x)), 3)), aes(label = SpawnBio_re,
    x = lims$x[2], y = lims$y[1]), hjust = "inward") +
  # Calculate the Mean Absolute relative error during fishing
  geom_text(data = aggregate(SpawnBio_re ~ A + AR + EM + time,
    data = subset(temp, time == "fishing"),
    function(x) round(mean(abs(x)), 3)), aes(label = SpawnBio_re,
    x = lims$x[1], y = lims$y[1]), hjust = "inward")
ggsave(file.path(fig_folder, "tsSSB.png"), g,
  height = height / 75, width = width / 50)

###############################################################################
###############################################################################
#### Figure tsf
###############################################################################
###############################################################################
#' Plot F internal versus external
#' for the maximum amount of data
x <- "year"; y <- "F_re"; z <- "AR"; zz <- "EM"
xlab <- "year"; ylab <- "Relative error in fishing mortality"
form <- ss3sim:::facet_form(zz, NULL, z, NULL)
temp <- droplevels(subset(ts,
  A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))

g <- ggplot(data = temp, aes_string(x = x)) +
  geom_boxplot(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(c(25, 80)) + ylim(c(-0.3, 0.3)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "tsf.png"), g,
  height = height / 75, width = width / 50)

###############################################################################
###############################################################################
#### Figure tsrec
###############################################################################
###############################################################################
#' Plot recruitment internal versus external
#' for the maximum amount of data
x <- "year"; y <- "Recruit_0_re"; z <- "AR"; zz <- "EM"
xlab <- "year"; ylab <- "Relative error in recruitment"
form <- ss3sim:::facet_form(zz, NULL, z, NULL)
temp <- droplevels(subset(ts,
  A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))

g <- ggplot(data = temp, aes_string(x = x)) +
  geom_boxplot(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  geom_vline(aes_string(xintercept = 80), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(c(25, 100)) + ylim(c(-0.5, 0.75)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "tsrec.png"), g,
  height = height / 75, width = width / 50)

###############################################################################
###############################################################################
#### Figure coveragessb
###############################################################################
###############################################################################
#' Plot autocorrelation internal versus external
x <- "year"; y <- "prop"; z <- "AR"; zz <- "EM"
xlab <- "year"; ylab <- "Forecast coverage of spawning biomass"
form <- ss3sim:::facet_form(zz, NULL, z, NULL)

temp <- droplevels(subset(ci,
  A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))
table <- aggregate(in.SPB_em ~ year + AR + EM,
    data = temp,
    function(x) c(sum(x), length(x), sum(x) / length(x)))
table <- data.frame(table[, -4], table[, 4])
colnames(table)[4:6] <- c("good", "total", "prop")
table$year <- as.numeric(as.character(table$year))

temp2 <- droplevels(subset(ci25,
  A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))
table2 <- aggregate(in.SPB_em ~ year + AR + EM,
    data = temp2, function(x) c(sum(x), length(x), sum(x) / length(x)))
table2 <- data.frame(table2[, -4], table2[, 4])
colnames(table2)[4:6] <- c("good", "total", "prop")
table2$year <- as.numeric(as.character(table$year))

g <- ggplot(table, aes_string(x = x)) +
  geom_point(aes_string(x = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  # geom_line(data = lines, aes_string(x = x, y = "diff")) +
  geom_hline(aes_string(yintercept = 0.5), linetype = "dashed", col = "red") +
  geom_hline(aes_string(yintercept = 0.75), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  geom_text(aes(x = -Inf, y = Inf, label = total),
    hjust = "inward", vjust = "inward") +
  theme +
  geom_point(data = table2, aes_string(x = x, y = y), pch = 21)
ggsave(file.path(fig_folder, "coveragessb.png"), g, height = height/75, width = width/50)

###############################################################################
###############################################################################
#### Figure replicate
###############################################################################
###############################################################################
png(file.path(fig_folder, "replicate.png"))
temp <- droplevels(subset(dq,
  EM == "Internal" & AR == " 0.90" & replicate == 2 & A %in% "A101" &
  SR_sigmaR_om == SDmarg & bias == "yes"))
temp <- temp[grepl("^[[:digit:]]", as.character(temp$year)), ]
temp$year <- as.numeric(as.character(temp$year))
temp$high <- temp$Value.SPB_em + 1.96 * temp$StdDev.SPB_em
temp$low <- temp$Value.SPB_em - 1.96 * temp$StdDev.SPB_em
temp <- droplevels(subset(temp, year > 25))
plot(temp$year, temp$high, ylim = c(0, max(temp$high)), type = "l",
  xlab = "year", ylab = "Spawning biomass for a single replicate",
  lty = 2)
legend("bottomleft", legend = "AR = 0.9", bty = "n")
lines(temp$year, temp$Value.SPB_em, type = "l")
lines(temp$year, temp$low, type = "l", lty = 2)
dev.off()

###############################################################################
###############################################################################
#### Figure true autocorrelation
###############################################################################
###############################################################################
x <- "year"; y <- "Value.SPB_em"; yy <- "Value.SPB_om"
z <- "AR"; zz <- "EM"
xlab <- "year"; ylab <- "Spawning biomass"
form <- ss3sim:::facet_form(zz, NULL, z, NULL)
temp <- droplevels(subset(dq,
  A %in% "A101" & EM %in% allem & SR_sigmaR_om == SDmarg &
  replicate == 2))
temp <- temp[grepl("^[[:digit:]]", as.character(temp$year)), ]
temp$year <- as.numeric(as.character(temp$year))
temp$high <- temp$Value.SPB_em + 1.96 * temp$StdDev.SPB_em
temp$low <- temp$Value.SPB_em - 1.96 * temp$StdDev.SPB_em

g <- ggplot(temp, aes_string(x = x)) +
  # geom_line(aes_string(group = x, y = y)) +
  geom_ribbon(aes_string(ymin = "low", ymax = "high"), fill = "red", alpha = 0.2) +
  geom_line(aes_string(x = x, y = yy), colour = "black") +
  geom_line(aes_string(x = x, y = y), colour = "red") +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = 25), linetype = "dashed", col = "black") +
  geom_vline(aes_string(xintercept = 80), linetype = "dashed", col = "black") +
  xlab(xlab) + ylab(ylab) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "tsSSB_replicate2.png"), g,
       height = height/75, width = width/50)

###############################################################################
###############################################################################
#### Figure sslx
#### 1 = peak, 3 = width
###############################################################################
###############################################################################
#' Plot autocorrelation internal versus external
x <- "SizeSel_2P_1_Survey_re"; y <- "SizeSel_2P_3_Survey_re"
z <- "AR"; zz <- "EM"
xlab <- "RE in survey selectivity peak"; ylab <- "RE in survey selectivity width"
form <- ss3sim:::facet_form(zz, NULL, z, NULL)
temp <- droplevels(subset(sc,
    A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))

g <- ggplot(temp, aes_string(x = x)) +
  geom_point(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  geom_vline(aes_string(xintercept = 0), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(c(-1, 1)) + ylim(c(-1, 1)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "sslx.png"), g, height = height/75, width = width/50)

###############################################################################
###############################################################################
#### Figure fslx
#### 1 = peak, 3 = width
###############################################################################
###############################################################################
#' Plot autocorrelation internal versus external
x <- "SizeSel_1P_1_Fishery_re"; y <- "SizeSel_1P_3_Fishery_re"
z <- "AR"; zz <- "EM"
xlab <- "RE in fishery selectivity peak"; ylab <- "RE in fishery selectivity width"
form <- ss3sim:::facet_form(zz, NULL, z, NULL)
temp <- droplevels(subset(sc,
    A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))

g <- ggplot(temp, aes_string(x = x)) +
  geom_point(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  geom_vline(aes_string(xintercept = 0), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(c(-1, 1)) + ylim(c(-1, 1)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "fslx.png"), g, height = height/75, width = width/50)

###############################################################################
###############################################################################
#### Figure estimatedAR
###############################################################################
###############################################################################
#' Plot autocorrelation for different amounts of data
x <- "nyears"; y <- "SR_autocorr_em"; z <- "AR"
xlab <- "Estimated autocorrelation"
form <- ss3sim:::facet_form(x, NULL, z, NULL)
temp <- droplevels(subset(sc,
    !grepl("A91", sc$A) & EM %in% c("Internal") & SR_sigmaR_om == SDmarg &
    A != "A104"))
temp$nyears <- factor(temp$A, levels = levels(temp$A),
  labels = nyears.lengthdata[-length(nyears.lengthdata)])

rline <- data.frame("AR" = rep(unique(temp$AR), 1),
  rep(unique(sc[sc$EM == "True", y]), 1))
colnames(rline)[2] <- y

g <- ggplot(temp) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = y), linetype = "dashed",
    data = rline, col = "red") +
  xlab(expression(rho)) +
  theme + theme(legend.position = c(0.08, 0.8)) +
  geom_text(data = aggregate(replicate ~ AR + nyears, data = temp,
    function(x) length(unique(x))), aes(label = replicate,
    x = -1, y = 100), vjust = "inward", hjust = "inward")
ggsave(file.path(fig_folder, "estimatedAR_ts.png"), g, height = 3.5, width = 8)

###############################################################################
###############################################################################
#### Figure estimatedAR for different amounts of data using zero_ext
###############################################################################
###############################################################################
#' Plot autocorrelation for different amounts of data
x <- "nyears"; y <- "SR_autocorr_em"; z <- "AR"
xlab <- "Estimated autocorrelation"
form <- ss3sim:::facet_form(x, NULL, z, NULL)
temp <- droplevels(subset(sc,
    !grepl("A91", sc$A) & EM %in% c("External") & SR_sigmaR_om == SDmarg &
    A != "A104"))
temp$nyears <- factor(temp$A, levels = levels(temp$A),
  labels = nyears.lengthdata[-length(nyears.lengthdata)])

rline <- data.frame("AR" = rep(unique(temp$AR), 1),
  rep(unique(sc[sc$EM == "True", y]), 1))
colnames(rline)[2] <- y

g <- ggplot(temp) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = y), linetype = "dashed",
    data = rline, col = "red") +
  xlab(expression(rho)) +
  theme + theme(legend.position = c(0.08, 0.8)) +
  geom_text(data = aggregate(replicate ~ AR + nyears, data = temp,
    function(x) length(unique(x))), aes(label = replicate,
    x = -1, y = 100), vjust = "inward", hjust = "inward")
ggsave(file.path(fig_folder, "estimatedAR_ts_zeroext.png"), g, height = 3.5, width = 8)

###############################################################################
###############################################################################
#### Figure estimatedAR
###############################################################################
###############################################################################
#' Plot autocorrelation for different amounts of data
x <- "EM"; y <- "SR_autocorr_em"; z <- "SR_sigmaR_om"
xlab <- "Estimated autocorrelation"
form <- ss3sim:::facet_form(x, NULL, z, NULL)
temp <- droplevels(subset(sc,
    A == "A101" & EM %in% keepem & AR == " 0.50"))

g <- ggplot(temp) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = 0.5), linetype = "dashed", col = "red") +
  xlab(expression(rho)) +
  theme + theme(legend.position = c(0.08, 0.8)) +
  geom_text(data = aggregate(replicate ~ AR + EM + SR_sigmaR_om, data = temp,
    function(x) length(unique(x))), aes(label = replicate,
    x = -1, y = 100), vjust = "inward")
ggsave(file.path(fig_folder, "estimatedAR_sigmaR.png"), g, height = 3.5, width = 8)

###############################################################################
###############################################################################
#### Table estimates of bias adjustment
###############################################################################
###############################################################################
temp <- droplevels(subset(sc,
    A == "A101" & EM %in% allem & SR_sigmaR_om == SDmarg))
write.csv(aggregate(as.matrix(temp[,
  c("last_yr_early_em", "first_yr_full_em", "last_yr_full_em",
    "first_yr_recent_em", "max_bias_adj_em")]) ~
  EM + AR, data = temp, mean), file.path(fig_folder, "bias_mean.csv"),
  row.names = FALSE)

###############################################################################
###############################################################################
#### r4ss plots
###############################################################################
###############################################################################
temp <- dir(pattern = "A101-D101")[1]
SS_plots(replist = SS_output(dir =
  file.path(temp, "1", "em"), ncols = 400),
  dir = temp)
file.copy(file.path(temp, "plots", "data_plot.png"),
  file.path(fig_folder, "data_plot.png"), overwrite = TRUE)
