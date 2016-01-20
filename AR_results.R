
#' Read in the results
getresults <- FALSE
if (getresults) {
  get_results_all(overwrite_files = TRUE, parallel = doparallel)
}
sc <- read.csv("ss3sim_scalar.csv")
ts <- read.csv("ss3sim_ts.csv")
sc$bias <- ifelse(sc$max_bias_adj_em == -1, "no", "yes")

#' Calculate relative error
sc <- calculate_re(sc, add = TRUE)
ts <- calculate_re(ts, add = TRUE)

#' Add max grad and par on bounds
ts <- data.frame(ts, sc[match(ts$ID, sc$ID), c("max_grad", "params_on_bound_em")])
# dq <- data.frame(dq, sc[match(dq$ID, sc$ID), c("max_grad", "params_on_bound_em")])

#' Calculate cis
# ci <- calulate_ci(dq[, ], verbose = TRUE)

#' Add names
sc$AR <- factAR(sc$species)
sc$EM <- factEM(sc$species)
ts$AR <- factAR(ts$species)
ts$EM <- factEM(ts$species)
# dq$AR <- factAR(dq$species)
# dq$EM <- factEM(dq$species)
# ci$AR <- factAR(ci$species)
# ci$EM <- factEM(ci$species)

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
   matplot(toplot, type = "l", las = 1, lty = 1:NCOL(toplot), col = "black",
     xlab = "year", ylab = "recruitment deviations", lwd = linewd)
   legend("topleft", legend = as.character(format(AR[sub], digits = 2)),
     lty = 1:length(sub), bty = "n", lwd = linewd, horiz = TRUE)
dev.off()

###############################################################################
###############################################################################
#### Figure estimatedAR
###############################################################################
###############################################################################
#' Plot autocorrelation internal versus external for the maximum
#' amount of data
x <- "EM"; y <- "SR_autocorr_em"; z <- "AR"
xlab <- "Estimated autocorrelation"
form <- ss3sim:::facet_form(x, NULL, z, NULL)
rline <- data.frame("EM" = rep(c("int", "int_ext", "zero_ext"), each = 6),
                    "AR" = rep(unique(sc$AR), 3),
                    rep(unique(sc[sc$EM == "true", y]), 3))
colnames(rline)[3] <- y

g <- ggplot(droplevels(subset(sc,
    A == "A101" &
    !EM %in% c("zero", "true") &
    max_grad < 0.01 &
    params_on_bound_em < 1 &
    SR_sigmaR_om == SDmarg))) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = y), linetype = "dashed",
    data = rline, col = "red") +
  xlab(expression(rho)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "estimatedAR.png"), g, height = 3.5, width = 8)

###############################################################################
###############################################################################
#### Figure estimatedlnr0
###############################################################################
###############################################################################
#' Plot autocorrelation internal versus external
#' for the maximum amount of data
x <- "EM"; y <- "SR_LN_R0_em"; z <- "AR"
lineval <- unique(sc$SR_LN_R0_om)
xlab <- expression(ln(r[0]))
form <- ss3sim:::facet_form(x, NULL, z, NULL)

g <- ggplot(droplevels(subset(sc,
  A == "A101" &
  max_grad < 0.01 & params_on_bound_em < 1))) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = lineval), linetype = "dashed", col = "red") +
  xlab(xlab) +
  theme + theme(legend.position = c(0.08, 0.8))
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

g <- ggplot(droplevels(subset(ts,
  A == "A101" &
  max_grad < 0.01 & params_on_bound_em < 1)), aes_string(x = x)) +
  geom_boxplot(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  geom_vline(aes_string(xintercept = 81), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(c(25, 100)) + ylim(c(-0.5, 0.75)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "tsSSB.png"), g, height = height/75, width = width/50)

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

g <- ggplot(droplevels(subset(ts,
  A == "A101" &
  max_grad < 0.01 & params_on_bound_em < 1)), aes_string(x = x)) +
  geom_boxplot(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(c(25, 100)) + ylim(c(-0.3, 0.3)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "tsf.png"), g, height = height/75, width = width/50)

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

g <- ggplot(droplevels(subset(ts,
  A == "A101" &
  max_grad < 0.01 & params_on_bound_em < 1)), aes_string(x = x)) +
  geom_boxplot(aes_string(group = x, y = y)) +
  facet_grid(form, scales = "fixed") +
  geom_hline(aes_string(yintercept = 0), linetype = "dashed", col = "red") +
  xlab(xlab) + ylab(ylab) +
  xlim(c(25, 100)) + ylim(c(-0.5, 0.75)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "tsrec.png"), g, height = height/75, width = width/50)

###############################################################################
###############################################################################
#### Figure coveragessb
###############################################################################
###############################################################################
# #' Plot autocorrelation internal versus external
# x <- "year"; y <- "prop"; z <- "AR"; zz <- "EM"
# xlab <- "year"; ylab <- "Forecast coverage of spawning stock biomass"
# form <- ss3sim:::facet_form(zz, NULL, z, NULL)

# table <- aggregate(in.SPB_em ~ year + AR + EM,
#     data = droplevels(subset(ci, max_grad < 0.01 & params_on_bound_em < 1)),
#     function(x) c(sum(x), length(x), sum(x) / length(x)))
# table <- data.frame(table[, -4], table[, 4])
# colnames(table)[4:6] <- c("good", "total", "prop")
# table$year <- as.numeric(as.character(table$year))

# temp <- ci
# temp$diff <- with(temp, as.numeric(high.SPB_em) - as.numeric(low.SPB_em))
# lines <- aggregate(diff ~ year + AR + EM,
#     data = droplevels(subset(temp, max_grad < 0.01 & params_on_bound_em < 1)),
#     function(x) {
#         y <- (x - max(x)) / max(x)
#         return(abs(mean(y)))
#     })
# lines$year <- as.numeric(as.character(lines$year))

# g <- ggplot(table, aes_string(x = x)) +
#   geom_point(aes_string(x = x, y = y)) +
#   facet_grid(form, scales = "fixed") +
#   # geom_line(data = lines, aes_string(x = x, y = "diff")) +
#   geom_hline(aes_string(yintercept = 0.5), linetype = "dashed", col = "red") +
#   xlab(xlab) + ylab(ylab) +
#   theme
# ggsave(file.path(fig_folder, "coveragessb.png"), g, height = height/75, width = width/50)

###############################################################################
###############################################################################
#### Figure replicate
###############################################################################
###############################################################################
# png(file.path(fig_folder, "replicate.png"))
# example <- droplevels(subset(dq,
#   EM == "internal" & AR == " 0.90" & replicate == 1))
# example$year <- as.numeric(as.character(example$year))
# example$high <- example$Value.SPB_em + 1.96 * example$StdDev.SPB_em
# example$low <- example$Value.SPB_em - 1.96 * example$StdDev.SPB_em
# example <- droplevels(subset(example, year > 25))
# plot(example$year, example$high, ylim = c(0, max(example$high)), type = "l",
#   xlab = "year", ylab = "Spawning stock biomass for a single replicate")
# legend("bottomleft", legend = "AR = 0.9", bty = "n")
# lines(example$year, example$Value.SPB_em, type = "l")
# lines(example$year, example$low, type = "l")
# dev.off()

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

g <- ggplot(droplevels(subset(sc,
  A == "A101" &
  max_grad < 0.01 & params_on_bound_em < 1)), aes_string(x = x)) +
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

g <- ggplot(droplevels(subset(sc,
  A == "A101" &
  max_grad < 0.01 & params_on_bound_em < 1)), aes_string(x = x)) +
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
rline <- data.frame("EM" = rep(c("int", "int_ext", "zero_ext"), each = 6),
                    "AR" = rep(unique(sc$AR), 3),
                    rep(unique(sc[sc$EM == "true", y]), 3))
colnames(rline)[3] <- y
sc$nyears <- factor(sc$A, levels = levels(sc$A),
  labels = c(nyears.lengthdata, levels(sc$A)[-c(1:length(nyears.lengthdata))]))
g <- ggplot(droplevels(subset(sc,
    !grepl("A91", sc$A) &
    EM %in% c("int") &
    max_grad < 0.01 &
    params_on_bound_em < 1 &
    SR_sigmaR_om == SDmarg))) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = y), linetype = "dashed",
    data = rline, col = "red") +
  xlab(expression(rho)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "estimatedAR_ts.png"), g, height = 3.5, width = 8)

###############################################################################
###############################################################################
#### Figure estimatedAR
###############################################################################
###############################################################################
#' Plot autocorrelation for different amounts of data
x <- "EM"; y <- "SR_autocorr_em"; z <- "SR_sigmaR_om"
xlab <- "Estimated autocorrelation"
form <- ss3sim:::facet_form(x, NULL, z, NULL)

g <-
ggplot(droplevels(subset(sc,
    A == "A101" &
    !EM %in% c("zero", "true") &
    max_grad < 0.01 &
    AR == " 0.50" &
    params_on_bound_em < 1))) +
  geom_histogram(aes_string(y), binwidth = 0.1) +
  facet_grid(form, scales = "fixed") +
  geom_vline(aes_string(xintercept = 0.5), linetype = "dashed", col = "red") +
  xlab(expression(rho)) +
  theme + theme(legend.position = c(0.08, 0.8))
ggsave(file.path(fig_folder, "estimatedAR_sigmaR.png"), g, height = 3.5, width = 8)
