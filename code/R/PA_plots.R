

# Set Date Range for plots.

dates <- seq.Date(as.Date("2016-09-24"), as.Date("2017-09-24"), by = "day")

# Party Names

plot_names <- c("CDU_CSU", "SPD", "Linke", "Grüne", "FDP", "AfD", "Andere")

# Party Colors and sort by data order

parties_col <- c("rgba(1,166,235,1)", "#000000", "#ffed00", "#46962b",  "rgba(120,0,57,1)", "#e3000f", "gray")
parties_col <- parties_col[c(2,6,5,4,3,1,7)]

df <- readRDS(paste0("results/draws_forcast_levels_2017_2_weaklyInformed.RDS"))


dfArray <- df$levels



adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), df$party_names)

dfArray <- dfArray[,adjustOrder,]
df$forcast <- df$forecast[,adjustOrder]

df$forcast <- dfArray[,,366]
# Calculate mean and 95 % Credible Interval for all parties/dates

means <- apply(dfArray,c(2,3), mean)
plot_df <- data.frame(date = dates, t(means))
colnames(plot_df)[2:8] <- plot_names

plot_df <- gather(plot_df, party, value, CDU_CSU:Andere, factor_key=TRUE)

tmp_hi <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 0.975))
tmp_hi <- data.frame(date = dates, t(tmp_hi))

tmp_hi <- gather(tmp_hi, party, high, X1:X7, factor_key=TRUE)

tmp_lo <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 0.025))
tmp_lo <- data.frame(date = dates, t(tmp_lo))

tmp_lo <- gather(tmp_lo, party, low, X1:X7, factor_key=TRUE)

plot_df$low <- tmp_lo$low
plot_df$high<- tmp_hi$high



tmp_hi <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 11/12))
tmp_hi <- data.frame(date = dates, t(tmp_hi))

tmp_hi <- gather(tmp_hi, party, high, X1:X7, factor_key=TRUE)

tmp_lo <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 1/12))
tmp_lo <- data.frame(date = dates, t(tmp_lo))

tmp_lo <- gather(tmp_lo, party, low, X1:X7, factor_key=TRUE)

plot_df$low56 <- tmp_lo$low
plot_df$high56 <- tmp_hi$high

# Multiply by 100 to get % values
plot_df[,3:7] <- plot_df[,3:7]*100

# Change name of CDU/CSU
levels(plot_df$party)[1] <- "CDU/CSU"

df_forecast <- data.frame(y = apply(df$forcast,2, mean),
                          ci = t(apply(df$forcast,2, function(x) quantile(x,c(1/12,11/12)))),
                          ci95 = t(apply(df$forcast,2, function(x) quantile(x,c(0.025,0.975)))))

rownames(df_forecast) <- plot_names
colnames(df_forecast) <- c("value", "low", "high", "low95", "high95")

df_forecast <- round(df_forecast*100, 1)

df_forecast$name <- c("CDU/CSU", plot_names[2:7])
df_forecast$name_eng <- c("CDU/CSU", "SPD", "Left", "Greens", "FDP", "AfD", "Others")
# df_forecast$color <- parties_col1
# df_forecast$color1 <- parties_col1
df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0,6,1)


dev.off()


pdf(file="forecast_res_PA_scale_comb_newfix.pdf",  height=5, width=8, family="URWTimes")
par(mar=c(5,5,0,0)+.1)
plot(x = c(1,2,3,4,5,6,7),
     y = df_forecast$value, col = "white", 
     type = "n", bty = "n", 
     ylim = c(0,50), xlim = c(0,7.5),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n",
     cex.axis = 1.2)
abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
abline(h = c(0), lty = "solid", col = "lightgrey")
abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))

# segments(y0 = c(33.7,  39.3), y1 = c(35.7, 39.3), x0 = c(0.5,  0.5), x1 = c(1, 0.9), lty = "dashed",  col = adjustcolor("black", alpha = 0.7))
# segments(y0 = c(30.9), y1 = c(32.9), x0 = c(1.35), x1 = c(1.15), lty = "dashed",  col = adjustcolor("black", alpha = 0.4))

segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.4), lend = 1)
#segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.5), lend = 1)
segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.7), lend = 1)
segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low, y1 = df_forecast$high, lwd = 20, col = adjustcolor("grey", alpha = 0.9), lend = 1)

points(x = c(1,2,3,4,5,6,7),
       y = df_forecast$value, col = "white", lwd = 2)
text(y = df_forecast$value, x = c(1,2,3,4,5,6,7)-0.35, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = c(1,2,3,4,5,6,7)+0.35, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
axis(1, at = c(1,2,3,4,5,6,7), labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
mtext("Vote Share (%)", side=2, line=3, cex=1.2)

# text(y = c(33.7,  39.3), x = 1-1, labels = c("Prediction",  "5/6 CI"), adj = 0, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
# text(y = c(30.9), x = 1+0.4, labels = c( "Result"), adj = 0, cex = 0.9,  col = adjustcolor("black", alpha = 0.4))
dev.off()


pdf(file="forecast_res_PA.pdf", height=7.56, width=12, family="URWTimes")
par(mar=c(5,5,0,0)+.1)
plot(x = c(1,2,3,4,5,6,7),
     y = df_forecast$value, col = "white", 
     type = "n", bty = "n", 
     ylim = c(0,50), xlim = c(0,7.5),
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n",
     cex.axis = 1.2)
abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
abline(h = c(0), lty = "solid", col = "lightgrey")
abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))

segments(y0 = c(33.7,  39.3), y1 = c(35.7, 39.3), x0 = c(0.5,  0.35), x1 = c(1, 0.9), lty = "dashed",  col = adjustcolor("black", alpha = 0.7))
segments(y0 = c(30.9), y1 = c(32.9), x0 = c(1.35), x1 = c(1.15), lty = "dashed",  col = adjustcolor("black", alpha = 0.4))

segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.4), lend = 1)
#segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.5), lend = 1)
segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low, y1 = df_forecast$high, lwd = 20, col = adjustcolor("grey", alpha = 0.7), lend = 1)


points(x = c(1,2,3,4,5,6,7),
       y = df_forecast$value, col = "white", lwd = 2)
text(y = df_forecast$value, x = c(1,2,3,4,5,6,7)-0.3, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = c(1,2,3,4,5,6,7)+0.3, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
axis(1, at = c(1,2,3,4,5,6,7), labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
mtext("Vote Share (%)", side=2, line=3, cex=1.2)

text(y = c(33.7,  39.3), x = 1-1, labels = c("Prediction",  "5/6 CI"), adj = 0, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
text(y = c(30.9), x = 1+0.4, labels = c( "Result"), adj = 0, cex = 0.9,  col = adjustcolor("black", alpha = 0.4))
dev.off()


# Figure 2

cutoffs <- c(2, 8, 36, 64, 92, 116, 148)

files <-  rownames(details)[c(1,5, 23, 36, 51, 66, 70)]

df <- readRDS(files[1])


adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), df$party_names)


df$forcast <- df$forcast[,adjustOrder]

df_forecast <- data.frame(y = apply(df$forcast,2, mean),
                          ci = t(apply(df$forcast,2, function(x) quantile(x,c(1/12,11/12)))),
                          ci95 = t(apply(df$forcast,2, function(x) quantile(x,c(0.025,0.975)))))

rownames(df_forecast) <- plot_names
colnames(df_forecast) <- c("value", "low", "high", "low95", "high95")

df_forecast <- round(df_forecast*100, 1)

df_forecast$name <- c("CDU/CSU", plot_names[2:7])
df_forecast$name_eng <- c("CDU/CSU", "SPD", "Left", "Greens", "FDP", "AfD", "Others")
df_forecast$t <- 2
# df_forecast$color <- parties_col1
# df_forecast$color1 <- parties_col1
df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0,6,1)


for(i in 2:length(cutoffs)){

df <- readRDS(files[i])


adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), df$party_names)


df$forcast <- df$forcast[,adjustOrder]

tmp_forecast <- data.frame(y = apply(df$forcast,2, mean),
                          ci = t(apply(df$forcast,2, function(x) quantile(x,c(1/12,11/12)))),
                          ci95 = t(apply(df$forcast,2, function(x) quantile(x,c(0.025,0.975)))))



rownames(tmp_forecast) <- plot_names
colnames(tmp_forecast) <- c("value", "low", "high", "low95", "high95")

tmp_forecast <- round(tmp_forecast*100, 1)

tmp_forecast$name <- c("CDU/CSU", plot_names[2:7])
tmp_forecast$name_eng <- c("CDU/CSU", "SPD", "Left", "Greens", "FDP", "AfD", "Others")
tmp_forecast$t <- cutoffs[i]
# df_forecast$color <- parties_col1
# df_forecast$color1 <- parties_col1
tmp_forecast$y <- tmp_forecast$value
tmp_forecast$x <- seq(0,6,1)

df_forecast <- rbind(df_forecast, tmp_forecast)
}
# Calculate mean and 95 % Credible Interval for all parties/dates

# load jags_oospreds_summ

load("../Modeling/structural_forecasts.RData")

struct_forecast <- jags_oospreds_summary_df[jags_oospreds_summary_df$election==2017,]

adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), struct_forecast$party)


struct_forecast <- struct_forecast[adjustOrder,]


parties <- unique(df_forecast$x)

election_res <- c(32.9,20.5, 9.2, 8.9, 10.7, 12.6, 5)



pdf(file="forecast_t_PA_neu.pdf", height=7.56, width=12, family="URWTimes")
par(mar=c(0,0,2,0)+.1)
par(oma=c(5,5,0,0)+.1)
layout(matrix(c(1,2,3,4,5,6), 1, 6, byrow = TRUE))

for(i in 1:6){
  sel <- parties[i]
  # ymin <- floor(min(df_forecast$low[df_forecast$x==sel])-1/4*min(df_forecast$low[df_forecast$x==sel]))
  # ymax <- ceiling(max(df_forecast$high[df_forecast$x==sel])+1/4*max(df_forecast$high[df_forecast$x==sel]))
  plot(x = 7:1, y = df_forecast$value[df_forecast$x==sel], 
       ylim = c(0, 45), type = "n",
       yaxt = "n", xaxt = "n", ylab = "", xlab = "", bty = "n", las = 1)
  abline(h = election_res[i])
  abline(h = struct_forecast$mean[i], lty = "dashed")
  segments(x0 = 7:1, y0 = df_forecast$low[df_forecast$x==sel], y1 = df_forecast$high[df_forecast$x==sel], col = adjustcolor("grey", alpha = 0.7), lwd = 6, lend = 1)
  points(x = 7:1, y = df_forecast$value[df_forecast$x==sel], col = "white")
  title(paste(df_forecast$name_eng[df_forecast$x==sel][1]), cex.main = 1.5)
  #if(i > 3) 
    axis(1, at = 7:1 , labels = paste(cutoffs), col = NA, col.ticks = 1)
  #if(i == 1 | i == 4) 
  if(i==1) axis(2, col = NA, col.ticks = 1, las = 1, cex.axis = 1.2)
  if(i>1) axis(2, col = "grey", col.ticks = "white", las = 1, labels = NA)
}

mtext("Days until election", 1, 3, outer=TRUE, cex = 1.2)
mtext("Vote Share (%)", 2, 3, outer=TRUE, cex = 1.2)
dev.off()


pdf(file="forecast_t_PA_neu_scale.pdf",height=4.13, width=5.83, family="URWTimes")
par(mar=c(0,0,2,0)+.1, family = "Gill Sans")
par(oma=c(5,5,0,0)+.1)
layout(matrix(c(1,2,3,4,5,6), 1, 6, byrow = TRUE))

for(i in 1:6){
  sel <- parties[i]
  # ymin <- floor(min(df_forecast$low[df_forecast$x==sel])-1/4*min(df_forecast$low[df_forecast$x==sel]))
  # ymax <- ceiling(max(df_forecast$high[df_forecast$x==sel])+1/4*max(df_forecast$high[df_forecast$x==sel]))
  plot(x = 7:1, y = df_forecast$value[df_forecast$x==sel], 
       ylim = c(0, 45), type = "n", xlim = c(0.7,7),
       yaxt = "n", xaxt = "n", ylab = "", xlab = "", bty = "n", las = 1)
  abline(h = election_res[i])
  abline(h = struct_forecast$mean[i], lty = "dashed")
  segments(x0 = 7:1, y0 = df_forecast$low[df_forecast$x==sel], y1 = df_forecast$high[df_forecast$x==sel], col = adjustcolor("grey", alpha = 0.7), lwd = 6, lend = 1)
  
  points(x = 1:7, y = avg_polls[,i], col = "red")
  points(x = 7:1, y = df_forecast$value[df_forecast$x==sel], col = "white")
  title(paste(df_forecast$name_eng[df_forecast$x==sel][1]), cex.main = 1.5)
  #if(i > 3) 
  axis(1, at = 7:1 , labels = paste(cutoffs), col = NA, col.ticks = 1, las = 2)
  #if(i == 1 | i == 4) 
  if(i==1) axis(2, col = NA, col.ticks = 1, las = 1, cex.axis = 1.2)
  if(i>1) axis(2, col = "grey", col.ticks = NA, las = 1, labels = NA)
}

mtext("Days until election", 1, 3, outer=TRUE, cex = 1.2)
mtext("Vote Share (%)", 2, 3, outer=TRUE, cex = 1.2)
dev.off()




pdf(file="forecast_t_PA.pdf", height=7.56, width=12, family="URWTimes")
par(mar=c(0,0,2,0)+.1)
par(oma=c(4,4,0,6)+.1)
layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))

for(i in 1:6){
sel <- parties[i]
plot(x = -df_forecast$t[df_forecast$x==sel], y = df_forecast$value[df_forecast$x==sel], 
     ylim = c(0,50), type = "n",
     yaxt = "n", xaxt = "n", ylab = "", xlab = "", bty = "n", las = 1)
abline(h = election_res[i])
abline(h = struct_forecast$mean[i], lty = "dashed")
segments(x0 = -df_forecast$t[df_forecast$x==sel], y0 = df_forecast$low[df_forecast$x==sel], y1 = df_forecast$high[df_forecast$x==sel], col = adjustcolor("grey", alpha = 0.7), lwd = 6, lend = 1)
points(x = -df_forecast$t[df_forecast$x==sel], y = df_forecast$value[df_forecast$x==sel], col = "white")
title(paste(df_forecast$name_eng[df_forecast$x==sel][1]))
if(i > 3) axis(1, at = -df_forecast$t[df_forecast$x==sel] , labels = paste(cutoffs), col = NA, col.ticks = 1)
if(i == 1 | i == 4) axis(2, col = NA, col.ticks = 1, las = 1 )
}

mtext("Days until election", 1, 2, outer=TRUE)
mtext("Vote Share (%)", 2, 2, outer=TRUE)
dev.off()

?axis



means <- apply(dfArray,c(2,3), mean)
plot_df <- data.frame(date = dates, t(means))
colnames(plot_df)[2:8] <- plot_names

plot_df <- gather(plot_df, party, value, CDU_CSU:Andere, factor_key=TRUE)

tmp_hi <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 0.975))
tmp_hi <- data.frame(date = dates, t(tmp_hi))

tmp_hi <- gather(tmp_hi, party, high, X1:X7, factor_key=TRUE)

tmp_lo <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 0.025))
tmp_lo <- data.frame(date = dates, t(tmp_lo))

tmp_lo <- gather(tmp_lo, party, low, X1:X7, factor_key=TRUE)

plot_df$low <- tmp_lo$low
plot_df$high<- tmp_hi$high



tmp_hi <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 11/12))
tmp_hi <- data.frame(date = dates, t(tmp_hi))

tmp_hi <- gather(tmp_hi, party, high, X1:X7, factor_key=TRUE)

tmp_lo <- apply(dfArray,c(2,3), function(x) quantile(x,probs = 1/12))
tmp_lo <- data.frame(date = dates, t(tmp_lo))

tmp_lo <- gather(tmp_lo, party, low, X1:X7, factor_key=TRUE)

plot_df$low56 <- tmp_lo$low
plot_df$high56 <- tmp_hi$high

# Multiply by 100 to get % values
plot_df[,3:7] <- plot_df[,3:7]*100

# Change name of CDU/CSU
levels(plot_df$party)[1] <- "CDU/CSU"



#plot df from homepage_plots

# Figure 1

dev.off()

pdf(file="2017_EPSA.pdf", height=5, width=10, family="URWTimes")
par(mar=c(0,0,2,0)+.1)
par(oma=c(4,4,0,6)+.1)
par(mfrow=c(2,3))
sel <- plot_df$party==levels(plot_df$party)[1]
plot(x = plot_df$date[sel], 
     y= plot_df$value[sel], 
     type = "l", 
     ylim = c(0,40), 
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     main = levels(plot_df$party)[1])
# points(x = -df_polls_party$days_to_election[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]], 
#        y = df_polls_party$support[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]/100,
#        pch = as.numeric(df_polls_party$institute[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]),
#        col = "grey")

polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low56[sel],rev(plot_df$high56[sel])),
        col = rgb(.8, .8, .8, .5),
        border = NA)

polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low[sel],rev(plot_df$high[sel])),
        col = rgb(.8, .8, .8, .3),
        border = NA)
lines(x = plot_df$date[sel], 
      y= plot_df$value[sel])
abline(h = seq(0,40,5), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(v = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
axis(2, at = seq(0,40,5), labels = seq(0,40,5),las = 1)
abline(h = df_res_for$mean[df_res_for$year==2017&df_res_for$party=="cdu"], col = "grey", lwd = 2, lty = "dashed")

sel <- plot_df$party==levels(plot_df$party)[2]
plot(x = plot_df$date[sel], 
     y= plot_df$value[sel], 
     type = "l", 
     ylim = c(0,40), 
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     main = levels(plot_df$party)[2])
# points(x = -df_polls_party$days_to_election[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]], 
#        y = df_polls_party$support[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]/100,
#        pch = as.numeric(df_polls_party$institute[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]),
#        col = "grey")

polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low56[sel],rev(plot_df$high56[sel])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low[sel],rev(plot_df$high[sel])),
        col = rgb(.8, .8, .8, .3),
        border = NA)
lines(x = plot_df$date[sel], 
      y= plot_df$value[sel])
abline(h = seq(0,40,5), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(v = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(h = df_res_for$mean[df_res_for$year==2017&df_res_for$party=="spd"], col = "grey", lwd = 2, lty = "dashed")

sel <- plot_df$party==levels(plot_df$party)[3]
plot(x = plot_df$date[sel], 
     y= plot_df$value[sel], 
     type = "l", 
     ylim = c(0,40), 
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     main = "Left")
# points(x = -df_polls_party$days_to_election[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]], 
#        y = df_polls_party$support[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]/100,
#        pch = as.numeric(df_polls_party$institute[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]),
#        col = "grey")

polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low56[sel],rev(plot_df$high56[sel])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low[sel],rev(plot_df$high[sel])),
        col = rgb(.8, .8, .8, .3),
        border = NA)
lines(x = plot_df$date[sel], 
      y= plot_df$value[sel])
abline(h = seq(0,40,5), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(v = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(h = df_res_for$mean[df_res_for$year==2017&df_res_for$party=="lin"], col = "grey", lwd = 2, lty = "dashed")

sel <- plot_df$party==levels(plot_df$party)[4]
plot(x = plot_df$date[sel], 
     y= plot_df$value[sel], 
     type = "l", 
     ylim = c(0,40), 
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     main = "Greens")
# points(x = -df_polls_party$days_to_election[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]], 
#        y = df_polls_party$support[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]/100,
#        pch = as.numeric(df_polls_party$institute[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]),
#        col = "grey")

polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low56[sel],rev(plot_df$high56[sel])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low[sel],rev(plot_df$high[sel])),
        col = rgb(.8, .8, .8, .3),
        border = NA)
lines(x = plot_df$date[sel], 
      y= plot_df$value[sel])
abline(h = seq(0,40,5), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(v = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
axis(2, at = seq(0,40,5), labels = seq(0,40,5),las = 1)
axis(1, at = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], labels = as.Date("2017-09-24")- plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)])
abline(h = df_res_for$mean[df_res_for$year==2017&df_res_for$party=="gru"], col = "grey", lwd = 2, lty = "dashed")

sel <- plot_df$party==levels(plot_df$party)[5]
plot(x = plot_df$date[sel], 
     y= plot_df$value[sel], 
     type = "l", 
     ylim = c(0,40), 
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     main = levels(plot_df$party)[5])
# points(x = -df_polls_party$days_to_election[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]], 
#        y = df_polls_party$support[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]/100,
#        pch = as.numeric(df_polls_party$institute[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]),
#        col = "grey")

polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low56[sel],rev(plot_df$high56[sel])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low[sel],rev(plot_df$high[sel])),
        col = rgb(.8, .8, .8, .3),
        border = NA)
lines(x = plot_df$date[sel], 
      y= plot_df$value[sel])
abline(h = seq(0,40,5), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(v = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(h = df_res_for$mean[df_res_for$year==2017&df_res_for$party=="fdp"], col = "grey", lwd = 2, lty = "dashed")
axis(1, at = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], labels = as.Date("2017-09-24")- plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)])

sel <- plot_df$party==levels(plot_df$party)[6]
plot(x = plot_df$date[sel], 
     y= plot_df$value[sel], 
     type = "l", 
     ylim = c(0,40), 
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     main = levels(plot_df$party)[6])
# points(x = -df_polls_party$days_to_election[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]], 
#        y = df_polls_party$support[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]/100,
#        pch = as.numeric(df_polls_party$institute[df_polls_party$cutoff==levels(df_plot_party$cutoff)[1]]),
#        col = "grey")

polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low56[sel],rev(plot_df$high56[sel])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
polygon(x = c(plot_df$date[sel],rev(plot_df$date[sel])),
        y = c(plot_df$low[sel],rev(plot_df$high[sel])),
        col = rgb(.8, .8, .8, .3),
        border = NA)
lines(x = plot_df$date[sel], 
      y= plot_df$value[sel])
abline(h = seq(0,40,5), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(v = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
abline(h = df_res_for$mean[df_res_for$year==2017&df_res_for$party=="afd"], col = "grey", lwd = 2, lty = "dashed")
axis(1, at = plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)], labels = as.Date("2017-09-24")- plot_df$date[sel][(as.Date("2017-09-24") - plot_df$date[sel]) %in% c(365,200,100,1)])

mtext("Days until election", 1, 2, outer=TRUE)
mtext("Vote Share (%)", 2, 2, outer=TRUE)
dev.off()




parties_col<- c("rgba(0,0,0,1)" , "rgba(227,0,15,1)" , "rgba(120, 0, 57, 1)", "rgba(70,150,43,1)", "rgba(255,237,0,1)", "rgba(1,166,235,1)", "rgba(233,233,233,1)")
parties_col1 <- c("rgba(0,0,0,0.5)" , "rgba(227,0,15,0.5)" , "rgba(120, 0, 57, 0.5)", "rgba(70,150,43,0.5)", "rgba(255,237,0,0.5)", "rgba(1,166,235,0.5)", "rgba(233,233,233,0.5)")

df_forecast <- data.frame(y = apply(df$forcast,2, mean),
                          ci = t(apply(df$forcast,2, function(x) quantile(x,c(1/12,11/12)))),
                          ci95 = t(apply(df$forcast,2, function(x) quantile(x,c(0.025,0.975)))))

rownames(df_forecast) <- plot_names
colnames(df_forecast) <- c("value", "low", "high", "low95", "high95")

df_forecast <- round(df_forecast*100, 1)

df_forecast$name <- c("CDU/CSU", plot_names[2:7])
df_forecast$name_eng <- c("CDU/CSU", "SPD", "Left", "Greens", "FDP", "AfD", "Others")
# df_forecast$color <- parties_col1
# df_forecast$color1 <- parties_col1
df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0,6,1)



hc_forecast <- highchart() %>%
  hc_title(text = "Die Wahlprognose von zweitstimme.org", style = list(lineHeight = 25, fontSize = "25px")) %>% 
  hc_subtitle(text = paste("Zuletzt aktualisiert:",gsub(" 0", "", format(as.Date(last_date), " %d. %m.%Y")))) %>%
  hc_chart(type = "errorbar",
           polar = FALSE) %>% 
  hc_xAxis(categories = df_forecast$name) %>% 
  hc_yAxis(labels = list(format = "{value} %"),  plotLines = list(
    list(label = list(text = "5% Hürde"),
         color = "black",
         width = 2,
         zIndex = 5,
         dashStyle = "ShortDot",
         value = 5))) %>%
  hc_add_series(data.frame(df_forecast, color = parties_col1),  name = "Vorhersage",  stemWidth = 30, whiskerLength = 0, showInLegend = FALSE, tooltip = list(
    pointFormat = '<span style="font-weight: bold">83% CI</span>: <b>{point.low}%-{point.high}%</b><br/>')) %>%
  hc_add_series(data.frame(df_forecast %>% select(-low, -high), color = parties_col), type = "scatter", name = "Prognose", marker = list(radius = 6, lineWidth = 2), showInLegend = FALSE, tooltip = list(
    pointFormat = '<span style="font-weight: bold">{point.name}</span>: <b>{point.y:.1f}%</b><br/>')) %>% 
  hc_tooltip(shared = TRUE) %>% 
  hc_add_theme(thm)

hc_forecast

#######

pdf(file="output/forecast_EPSA.pdf", height=5, width=10, family="URWTimes")
par(mar=c(5,5,0,0)+.1)
plot(y = c(1,2,3,4,5,6,7),
     x = rev(df_forecast$value), col = "white", 
     type = "n", bty = "n", 
     xlim = c(0,50), ylim = c(0.5,7),
     ylab = "",
     xlab = "Predicted Vote Share (%)",
     yaxt = "n",
     xaxt = "n")
abline(v = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
abline(v = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))
segments(y0 = c(1,2,3,4,5,6,7), x0 = rev(df_forecast$low95), x1 = rev(df_forecast$high95), lwd = 10, col = adjustcolor("grey", alpha = 0.5), lend = 1)
segments(y0 = c(1,2,3,4,5,6,7), x0 = rev(df_forecast$low), x1 = rev(df_forecast$high), lwd = 10, col = adjustcolor("grey", alpha = 0.7), lend = 1)

points(y = c(1,2,3,4,5,6,7),
       x = rev(df_forecast$value), col = "white", lwd = 2)
text(x = rev(df_forecast$value), y = c(1,2,3,4,5,6,7)-0.4, labels = rev(df_forecast$value), cex = 0.7)
axis(2, at = c(1,2,3,4,5,6,7), labels = rev(df_forecast$name_eng), las = 1, tick = 0, cex.axis = 0.8)
axis(1, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), tick = 0)
dev.off()


par(mar=c(5,5,0,0)+.1)
plot(x = c(1,2,3,4,5,6,7),
     y = df_forecast$value, col = "white", 
     type = "n", bty = "n", 
     ylim = c(0,50), xlim = c(0,7.5),
     xlab = "",
     ylab = "Vote Share (%)",
     yaxt = "n",
     xaxt = "n")
abline(h = c(10,20,30,40,50), lty = "dashed", col = "lightgrey")
abline(h = c(5,15,25,35,45), lty = "dashed", col = adjustcolor("lightgrey", alpha = 0.5))

segments(y0 = c(33.7, 30.9, 39.3, 43.5), y1 = c(35.7, 32.9, 39.3, 43.5), x0 = c(0.5, 0.35, 0.35, 0.35), x1 = c(1,0.85, 0.9, 0.9), lty = "dashed",  col = adjustcolor("black", alpha = 0.7))

segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.4), lend = 1)
segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.5), lend = 1)
segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low, y1 = df_forecast$high, lwd = 20, col = adjustcolor("grey", alpha = 0.7), lend = 1)


points(x = c(1,2,3,4,5,6,7),
       y = df_forecast$value, col = "white", lwd = 2)
text(y = df_forecast$value, x = c(1,2,3,4,5,6,7)-0.3, labels = df_forecast$value, cex = 0.8,  col = adjustcolor("black", alpha = 0.7))
text(y = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), x = c(1,2,3,4,5,6,7)+0.3, labels = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5.0), cex = 0.8, col = adjustcolor("black", alpha = 0.4))
axis(1, at = c(1,2,3,4,5,6,7), labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 0.8)
axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0)


text(y = c(33.7, 30.9, 39.3, 43.5), x = 1-1, labels = c("Prediction", "Result", "5/6 CI", "95% CI"), adj = 0, cex = 0.8,  col = adjustcolor("black", alpha = 0.7))


# Plot without 95% CI








install.packages("formattable")

library(formattable)

# mtcars (mpg background in gradient: the higher, the redder)
as.htmlwidget(
  formattable(mtcars, list(mpg = formatter("span",
                                           style = x ~ style(display = "block",
                                                             "border-radius" = "4px",
                                                             "padding-right" = "4px",
                                                             color = "white",
                                                             "background-color" = rgb(x/max(x), 0, 0))))
  )
)
# since an htmlwidget, composes well with other tags

install.packages("tikzDevice")
library(tikzDevice)

tikz('normal.tex', standAlone = TRUE, width=5, height=5)

# Normal distribution curve
x <- seq(-4.5,4.5,length.out=100)
y <- dnorm(x)

# Integration points
xi <- seq(-2,2,length.out=30)
yi <- dnorm(xi)

# plot the curve
plot(x,y,type='l',col='blue',ylab='$p(x)$',xlab='$x$')
# plot the panels
lines(xi,yi,type='s')
lines(range(xi),c(0,0))
lines(xi,yi,type='h')

#Add some equations as labels
title(main="$p(x)=\\frac{1}{\\sqrt{2\\pi}}e^{-\\frac{x^2}{2}}$")
int <- integrate(dnorm,min(xi),max(xi),subdivisions=length(xi))
text(2.8, 0.3, paste("\\small$\\displaystyle\\int_{", min(xi),
                     "}^{", max(xi), "}p(x)dx\\approx", round(int[['value']],3),
                     '$', sep=''))

#Close the device
dev.off()

n <- 200
pie(rep(1, n), labels = "", col = rainbow(n), border = NA,
    main = "pie(*, labels=\"\", col=rainbow(n), border=NA,..")

## Another case showing pie() is rather fun than science:
## (original by FinalBackwardsGlance on http://imgur.com/gallery/wWrpU4X)
prob <- 35
tikz('20.tex', standAlone = F, width=1, height=1)
par(mar=c(0,0,0,0)+.1)
par(oma=c(0,0,0,0)+.1)
pie(c(prob,100-prob),
    init.angle = 90, col = c("black","white"), border = FALSE, clockwise = T, labels = NA)
dev.off()
