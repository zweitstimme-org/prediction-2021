df <- readRDS(file="output/nz/draws/combined_model/draws_forcast_levels_2017_2.RDS")

forecast <- df$forecast

adjustOrder <- match(c("nat", "lab", "gre", "nzf", "oth"), df$party_names)


forecast <- forecast[,adjustOrder]


# Party Names

plot_names <- c("National", "Labour", "Green", "NZ First", "Others")

df_forecast <- data.frame(y = apply(forecast, 2,  mean),
                          ci = t(apply(forecast, 2, function(x) quantile(x, c(1/12, 11/12)))),
                          ci95 = t(apply(forecast,2, function(x) quantile(x, c(0.025, 0.975)))))

rownames(df_forecast) <- plot_names
colnames(df_forecast) <- c("value", "low", "high", "low95", "high95")

df_forecast <- round(df_forecast*100, 1)


df_forecast$name <- plot_names
df_forecast$name_eng <- plot_names

df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0, 4, 1)

par(mar=c(5,5,0,0)+.1)
plot(x = c(1,2,3,4,5),
     y = df_forecast$value, col = "white", 
     type = "n", bty = "n", 
     ylim = c(0,55), xlim = c(0,5.5),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n",
     cex.axis = 1.2)
abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
abline(h = c(0), lty = "solid", col = "lightgrey")
abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))

segments(x0 = c(1,2,3,4,5), y0 = 0, y1 = c(44.45, 36.89, 6.27, 7.2, 5.19), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
segments(x0 = c(1,2,3,4,5), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.6), lend = 1)
segments(x0 = c(1,2,3,4,5), y0 = df_forecast$low, y1 = df_forecast$high, lwd = 20, col = adjustcolor("grey", alpha = 0.99), lend = 1)

points(x = c(1,2,3,4,5),
       y = df_forecast$value, col = "white", lwd = 2)
text(y = df_forecast$value, x = c(1,2,3,4,5)-0.25, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
text(y = c(44.45, 36.89, 6.27, 7.2, 5.19), x = c(1,2,3,4,5)+0.3, labels = c(44.45, 36.89, 6.27, 7.2, 5.19), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
axis(1, at = c(1,2,3,4,5), labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
mtext("Vote Share (%)", side=2, line=3, cex=1.2)

