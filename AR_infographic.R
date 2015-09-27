fval <- ss3sim:::get_args("cases/F0-cod.txt")

png("AR-perf-testing_recdevs.png", width = 6, height = 3, units = "in", res = 300)
par(mfrow = c(2, 1), las = 1, oma = c(3.2, 0, 0, 0), mar = c(0, 3, 0, 1), xpd = TRUE)
plot(exp(Eps[,1]), type = "o", xlab = "", ylab = "", xaxt = "n")
mtext(side = 3, line = -1.25, text = "Recruitment", cex = 1.15)
plot(fval$years, fval$fvals, type = "l", xlab = "", ylab = "")
mtext(side = 1, line = 2, text = "years")
mtext(side = 3, line = -1.5, text = "Fishing mortality", cex = 1.15)
dev.off()

report <- SS_output("copies\\A31-B0-D30-E100-F0-L32-cod\\1\\om",
  ncols = 500, covar = FALSE, compfile = "none", NoCompOK = TRUE)
SS_plots(report, uncertainty = FALSE, plot = c(1, 3:5))
reporte <- SS_output("copies\\A31-B0-D30-E100-F0-L32-cod\\1\\em",
  ncols = 500, covar = FALSE, compfile = "none", NoCompOK = TRUE)
SS_plots(reporte, uncertainty = FALSE, plot = c(1, 3))