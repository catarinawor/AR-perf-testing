#' Plot recruitment for each species

   png(file.path(fig_folder, paste0(spp, "recdevs_iteration001.png")),
     width = width, height = height, res = resolution, units = unit)
   toplot <- sapply(EpsList, function(x) x[, 1])
   matplot(toplot, type = "l", las = 1, lty = 1:length(AR), col = "black",
     xlab = "year", ylab = "recruitment deviations")
   legend("topleft", legend = as.character(format(AR, digits = 2)),
     lty = 1:length(AR), bty = "n")
   dev.off()

   png(file.path(fig_folder, paste0(spp, "biasadjustmentcheck.png")),
     width = width, height = height, res = resolution, units = unit)
     par(mfrow = c(2, 1), mar = c(0, 4, 0, 0.25), oma = c(5, 1, 1, 1))
     matplot(sapply(EpsList, colMeans), ylim = c(-0.25, 1.25), xaxt = "n",
       pch = 21, ylab = "mean rec dev", col = "black")
     abline(h = 0)
     matplot(sapply(lapply(EpsList, exp), colMeans), ylim = c(-0.25, 1.25),
       ylab = "mean exponentiated rec dev", pch = 21, col = "black")
     abline(h = 1)
     mtext(side = 1, line = 2.5, "iteration")
   dev.off()
