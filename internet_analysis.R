# Global Internet Usage Growth Analysis and Prediction Using R
# Student: Prasanth R | Roll No: 717825Z140
set.seed(42)
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)
sink("output/console_output.txt", split = TRUE)

cat("\n=== 1. LOAD DATA ===\n")
net <- read.csv("data/internet_usage.csv", stringsAsFactors = FALSE)
print(head(net))
cat("\nDimensions:\n"); print(dim(net)); print(names(net))
cat("\nStructure:\n"); str(net)
cat("\nSummary:\n"); print(summary(net))

cat("\n=== 2. DATA CLEANING ===\n")
cat("Missing values per column:\n"); print(colSums(is.na(net)))
cat("Duplicate rows:", sum(duplicated(net)), "\n")

cat("\n=== 3. FEATURE ENGINEERING ===\n")
net <- net[order(net$Entity, net$Year), ]
net$Growth <- ave(net$InternetUsers_pct, net$Entity, FUN = function(x) c(NA, diff(x)))
growth_label <- function(g) {
  if (is.na(g)) "Unknown" else if (g < 1) "Slow" else if (g < 3) "Moderate" else "Rapid"
}
net$GrowthClass <- factor(sapply(net$Growth, growth_label))
print(head(net[net$Entity == "World", ], 5))
print(table(net$GrowthClass))

world <- net[net$Entity == "World", ]
cat("\n=== 4. EXPLORATORY ANALYSIS ===\n")
cat("World: ", world$InternetUsers_pct[1], "% (2000) ->",
    tail(world$InternetUsers_pct, 1), "% (2023)\n")
latest <- net[net$Year == 2023 & net$Entity != "World", ]
print(latest[order(-latest$InternetUsers_pct), c("Entity", "InternetUsers_pct")])
cat("\nCorrelation matrix (World):\n")
print(round(cor(world[, c("Year", "InternetUsers_pct", "Growth")], use = "complete.obs"), 3))

png("output/plots/01_world_trend.png", 900, 550)
plot(world$Year, world$InternetUsers_pct, type = "b", pch = 19, col = "#0F766E", lwd = 2,
     xlab = "Year", ylab = "% of population online", main = "World Internet Usage, 2000-2023")
grid(); dev.off()

png("output/plots/02_country_trends.png", 900, 550)
ents <- setdiff(unique(net$Entity), "World"); cols <- rainbow(length(ents))
plot(NA, xlim = c(2000, 2023), ylim = c(0, 100), xlab = "Year", ylab = "% online",
     main = "Internet Usage by Country")
for (i in seq_along(ents)) { d <- net[net$Entity == ents[i], ]; lines(d$Year, d$InternetUsers_pct, col = cols[i], lwd = 2) }
legend("topleft", ents, col = cols, lwd = 2, cex = 0.7, ncol = 2); dev.off()

png("output/plots/03_bar_2023.png", 900, 550)
l <- latest[order(latest$InternetUsers_pct), ]
par(mar = c(5, 9, 4, 2))
barplot(l$InternetUsers_pct, names.arg = l$Entity, horiz = TRUE, las = 1, col = "#0F766E",
        xlab = "% online in 2023", main = "Internet Users by Country, 2023"); dev.off()

png("output/plots/04_growth_rate.png", 900, 550)
barplot(world$Growth[-1], names.arg = world$Year[-1], col = "#F59E0B", las = 2,
        ylab = "Percentage-point gain per year", main = "World: Yearly Growth in Internet Usage"); dev.off()

png("output/plots/05_boxplot.png", 900, 550)
boxplot(InternetUsers_pct ~ Year, data = net[net$Year %in% c(2000, 2005, 2010, 2015, 2020, 2023), ],
        col = "#99F6E4", xlab = "Year", ylab = "% online", main = "Spread Across Countries"); dev.off()

cat("\n=== 5. CHRONOLOGICAL TRAIN / TEST SPLIT ===\n")
n <- nrow(world); n_train <- 19
train <- world[1:n_train, ]; test <- world[(n_train + 1):n, ]
cat("Train:", min(train$Year), "-", max(train$Year), "(", nrow(train), "rows)\n")
cat("Test :", min(test$Year), "-", max(test$Year), "(", nrow(test), "rows)\n")

cat("\n=== 6. MODELS ===\n")
m_lin  <- lm(InternetUsers_pct ~ Year, data = train)
m_poly <- lm(InternetUsers_pct ~ poly(Year, 2), data = train)
m_log  <- lm(log(InternetUsers_pct) ~ Year, data = train)
m_sig  <- nls(InternetUsers_pct ~ SSlogis(Year, Asym, xmid, scal), data = train)
cat("\n-- Linear --\n");     print(summary(m_lin))
cat("\n-- Polynomial (deg 2) --\n"); print(summary(m_poly))
cat("\n-- Log-linear --\n"); print(summary(m_log))
cat("\n-- Logistic (S-curve) --\n"); print(summary(m_sig))

pred <- data.frame(Year = test$Year, Actual = test$InternetUsers_pct,
  Linear = predict(m_lin, test), Polynomial = predict(m_poly, test),
  LogLinear = exp(predict(m_log, test)), Logistic = predict(m_sig, test))
cat("\nPredictions on held-out years:\n"); print(round(pred, 2))

metrics <- function(a, p) c(RMSE = sqrt(mean((a - p)^2)), MAE = mean(abs(a - p)),
                            R2 = 1 - sum((a - p)^2) / sum((a - mean(a))^2))
res <- rbind(Linear = metrics(pred$Actual, pred$Linear),
             Polynomial = metrics(pred$Actual, pred$Polynomial),
             LogLinear = metrics(pred$Actual, pred$LogLinear),
             Logistic = metrics(pred$Actual, pred$Logistic))
cat("\n=== 7. MODEL EVALUATION (test set) ===\n"); print(round(res, 3))
write.csv(round(res, 3), "output/model_metrics.csv")
cat("Best model by RMSE:", rownames(res)[which.min(res[, "RMSE"])], "\n")

png("output/plots/06_actual_vs_pred.png", 900, 550)
plot(world$Year, world$InternetUsers_pct, pch = 19, col = "grey30", xlab = "Year",
     ylab = "% online", main = "Actual vs Predicted (test years 2019-2023)")
abline(v = max(train$Year) + 0.5, lty = 2)
cl <- c("#DC2626", "#2563EB", "#F59E0B", "#0F766E")
for (i in 1:4) lines(pred$Year, pred[, i + 2], col = cl[i], lwd = 2, type = "b")
legend("topleft", c("Actual", names(pred)[3:6]), col = c("grey30", cl), pch = c(19, 1, 1, 1, 1), lwd = c(NA, 2, 2, 2, 2)); dev.off()

png("output/plots/07_residuals.png", 900, 450)
par(mfrow = c(1, 2)); plot(m_lin, which = 1); plot(m_lin, which = 2); dev.off()

cat("\n=== 8. FORECAST TO 2030 (final model refit on all data) ===\n")
m_sig_all <- nls(InternetUsers_pct ~ SSlogis(Year, Asym, xmid, scal), data = world)
m_lin_all <- lm(InternetUsers_pct ~ Year, data = world)
fy <- data.frame(Year = 2024:2030)
fc <- data.frame(Year = fy$Year, Logistic = predict(m_sig_all, fy), Linear = predict(m_lin_all, fy))
print(round(fc, 1)); write.csv(round(fc, 2), "output/forecast_2030.csv", row.names = FALSE)
cat("Logistic saturation level:", round(coef(m_sig_all)["Asym"], 1), "%\n")

png("output/plots/08_forecast.png", 900, 550)
plot(world$Year, world$InternetUsers_pct, pch = 19, xlim = c(2000, 2030), ylim = c(0, 130),
     xlab = "Year", ylab = "% online", main = "Forecast to 2030")
lines(fc$Year, fc$Logistic, col = "#0F766E", lwd = 2, type = "b")
lines(fc$Year, fc$Linear, col = "#DC2626", lwd = 2, type = "b")
abline(h = 100, lty = 3)
legend("topleft", c("Actual", "Logistic", "Linear"), col = c("black", "#0F766E", "#DC2626"), pch = c(19, 1, 1)); dev.off()
sink()
