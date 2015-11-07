
library(ss3sim)
ignore <- sapply(list.files("R", full.names = TRUE), source)

#' Read in the files
sc <- read.csv("ss3sim_scalar.csv")
ts <- read.csv("ss3sim_ts.csv")

sc <- droplevels(subset(sc, species != "ste"))
ts <- droplevels(subset(ts, species != "ste"))

#' Add names
sc$AR <- factAR(sc$species)
sc$EM <- factEM(sc$species)
ts$AR <- factAR(ts$species)
ts$EM <- factEM(ts$species)

sc$SR_autocorr_om <- as.numeric(as.character(sc$AR))
ts$SR_autocorr_om <- as.numeric(as.character(ts$AR))

rmse <- aggregate(cbind(dev_om, dev_em) ~ ID, data = ts, function(x) {
  need <- x[1:80]
  sqrt(mean(need^2))
})
colnames(rmse) <- gsub("dev", "rmse", colnames(rmse))

sc <- merge(sc,
  droplevels(subset(ts, year == 80, select = c("ID", "SpawnBio_om", "SpawnBio_em"))))
sc <- merge(sc, rmse)
sc <- calculate_re(sc, add = TRUE)
ts <- calculate_re(ts, add = TRUE)
ts <- merge(ts, droplevels(sc[, c("ID", "max_grad")]))

write.csv(sc, "AR_results4RM.csv", row.names = FALSE)
write.csv(ts, "AR_results4RM_ts.csv", row.names = FALSE)

# Boxplot of relative error in AR per AR level
plot_scalar_boxplot(sc, x = "EM", y = "SR_autocorr_re", horiz = "AR", relative.error = TRUE)
# Plot of AR estimate x total NLL, colored by recruitment NLL
plot_scalar_points(sc, x = "NLL_TOTAL_em", y = "SR_autocorr_em", horiz = "AR", vert = "EM", color = "NLL_Recruitment_em")
# Plot of RE in AR by EM and true AR level, colored by maximum gradient
plot_scalar_points(sc, y = "SR_autocorr_re", x = "EM", horiz = "AR", color = "max_grad", relative.error = TRUE)
plot_scalar_points(sc, y = "rmse_em", x = "EM", horiz = "AR", color = "NLL_Recruitment_em")
