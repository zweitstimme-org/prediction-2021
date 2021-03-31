### ----------------------------------------------------------
### Election polling forecasting 
### implementation of backward random walk for multi-party set-ups
### 
### Lukas Stoetzer & Marcel Neunhoeffer
### 
### Post Estimation Code for Structural and Combined Model
###
### ----------------------------------------------------------

source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions

# Final Forecast in Text

df <- readRDS(file="../../output/ger/draws/combined_model/draws_forcast_levels_2017_2.RDS")

forecast <- df$forecast

adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), df$party_names)


forecast <- forecast[,adjustOrder]

round(apply(forecast, 2, mean)*100,1)

round(t(apply(forecast, 2, quantile, c(1/12, 11/12)))*100,1)

election_res <- c(32.9,20.5, 9.2, 8.9, 10.7, 12.6, 5)

round(sqrt(mean((round(apply(forecast, 2, mean)*100,1) - election_res)^2)),3)


# Probabilities in text

# Probability of 7 (counting CDU and CSU as two parties) parties entering parliament
sum(apply(forecast[,1:6] >= 0.05, 1, all)) / nrow(forecast) * 100

# Probability for AfD to become third strongest party
sum(t(apply(-forecast[,1:6], 1, rank))[,6] == 3) / nrow(forecast) * 100

# Probability for Left to become third strongest party
sum(t(apply(-forecast[,1:6], 1, rank))[,3] == 3) / nrow(forecast) * 100

# Probability for Greens to become third strongest party
sum(t(apply(-forecast[,1:6], 1, rank))[,4] == 3) / nrow(forecast) * 100

# Probability for FDP to become third strongest party
sum(t(apply(-forecast[,1:6], 1, rank))[,5] == 3) / nrow(forecast) * 100

# Coalition Probabilities

coa_df <- forecast

# Set share of parties below threshold to 0

coa_df[coa_df[,1:6] < 0.05] <- 0


# Majority for Grand Coalition? 
# (When coalition vote share is greater than half of the sum of the vote shares
# of parties anbove the 5% thershold.)

gc <- (coa_df[,1] + coa_df[,2]) > (apply(coa_df[,1:6], 1, sum) / 2)

sum(gc) / nrow(forecast) * 100

# Majority for Jamaica Coalition?

jam <- (coa_df[,1] + coa_df[,4] + coa_df[,5]) > (apply(coa_df[,1:6], 1, sum) / 2)

sum(jam) / nrow(forecast) * 100

# Majority for Black-Yellow?

by <- (coa_df[,1] + coa_df[,5]) > (apply(coa_df[,1:6], 1, sum) / 2)

sum(by) / nrow(forecast) * 100

# Majority for Black-Green?

bg <- (coa_df[,1] + coa_df[,4]) > (apply(coa_df[,1:6], 1, sum) / 2)

sum(bg) / nrow(forecast) * 100

# Majority for Red-Red-Green?

rrg <- (coa_df[,2] + coa_df[,3] + coa_df[,4]) > (apply(coa_df[,1:6], 1, sum) / 2)

sum(rrg) / nrow(forecast) * 100

# Figure 1

# Party Names

plot_names <- c("CDU_CSU", "SPD", "Linke", "Grüne", "FDP", "AfD", "Andere")

df_forecast <- data.frame(y = apply(forecast, 2,  mean),
                          ci = t(apply(forecast, 2, function(x) quantile(x, c(1/12, 11/12)))),
                          ci95 = t(apply(forecast,2, function(x) quantile(x, c(0.025, 0.975)))))

rownames(df_forecast) <- plot_names
colnames(df_forecast) <- c("value", "low", "high", "low95", "high95")

df_forecast <- round(df_forecast*100, 1)

df_forecast$name <- c("CDU/CSU", plot_names[2:7])
df_forecast$name_eng <- c("CDU/CSU", "SPD", "Left", "Greens", "FDP", "AfD", "Others")

df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0, 6, 1)


pdf(file="../../output/ger/plots/combined_model/figure1.pdf",  height=5, width=8, family="URWTimes")
par(mar=c(5,5,0,0)+.1)
plot(x = c(1,2,3,4,5,6,7),
     y = df_forecast$value, col = "white", 
     type = "n", bty = "n", 
     ylim = c(0,45), xlim = c(0,7.5),
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

segments(x0 = c(1,2,3,4,5,6,7), y0 = 0, y1 = c(32.9,20.5, 9.2, 8.9, 10.7, 12.6,5), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
#segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.5), lend = 1)
segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.6), lend = 1)
segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low, y1 = df_forecast$high, lwd = 20, col = adjustcolor("grey", alpha = 0.99), lend = 1)

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

# Figure 2

Election <- 2017


cutoffs <- c(2, 8, 36, 64, 92, 116, 148)

df_forecast <- NULL


for(cutoff in cutoffs){
  
  file <- list.files(path = "../../output/ger/draws/combined_model", 
                     pattern = paste0("draws*.*_",Election,"_",cutoff,".RDS"), 
                     full.names = T)
  
  df <- readRDS(file)
  
  
  adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), df$party_names)
  
  
  forecast <- df$forecast[,adjustOrder]
  
  tmp_forecast <- data.frame(y = apply(forecast, 2, mean),
                             ci = t(apply(forecast, 2, function(x) quantile(x, c(1/12, 11/12)))),
                             ci95 = t(apply(forecast, 2, function(x) quantile(x, c(0.025, 0.975)))))
  
  
  
  rownames(tmp_forecast) <- plot_names
  colnames(tmp_forecast) <- c("value", "low", "high", "low95", "high95")
  
  tmp_forecast <- round(tmp_forecast*100, 1)
  
  tmp_forecast$name <- c("CDU/CSU", plot_names[2:7])
  tmp_forecast$name_eng <- c("CDU/CSU", "SPD", "Left", "Greens", "FDP", "AfD", "Others")
  tmp_forecast$t <- cutoff
  # df_forecast$color <- parties_col1
  # df_forecast$color1 <- parties_col1
  tmp_forecast$y <- tmp_forecast$value
  tmp_forecast$x <- seq(0, 6 ,1)
  
  df_forecast <- rbind(df_forecast, tmp_forecast)
}

# Get polling averages

file <- list.files(path = "../../output/ger/draws/combined_model", 
                   pattern = paste0("draws*.*_",2017,"_",2,".RDS"), 
                   full.names = T)

df <- readRDS(file)

avg_polls <- matrix(NA, nrow = length(cutoffs), ncol = 7)
df_polls <- df$polls
for(i in 1:length(cutoffs)){
  avg_polls[i,] <- apply(df_polls[(df_polls$days_to_election >= cutoffs[i] & df_polls$days_to_election < cutoffs[i]+35),][,6:ncol(df_polls)],2,mean)
}

colnames(avg_polls) <- names(df_polls[6:ncol(df_polls)])
rownames(avg_polls) <- cutoffs

adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), colnames(avg_polls))

avg_polls <- avg_polls[,adjustOrder]

struct_forecast <- readRDS(paste0("../../output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))


adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), colnames(struct_forecast))


struct_forecast <- struct_forecast[,adjustOrder]

parties <- unique(df_forecast$x)

election_res <- c(32.9,20.5, 9.2, 8.9, 10.7, 12.6, 5)


pdf(file="../../output/ger/plots/combined_model/figure2.pdf", height=5, width=8, family="URWTimes")
par(mar=c(0,0,2,0)+.1)
par(oma=c(5,5,0,0)+.1)
layout(matrix(c(1,2,3,4,5,6), 1, 6, byrow = TRUE))

for(i in 1:6){
  sel <- parties[i]
  # ymin <- floor(min(df_forecast$low[df_forecast$x==sel])-1/4*min(df_forecast$low[df_forecast$x==sel]))
  # ymax <- ceiling(max(df_forecast$high[df_forecast$x==sel])+1/4*max(df_forecast$high[df_forecast$x==sel]))
  plot(x = 7:1, y = df_forecast$value[df_forecast$x==sel], 
       ylim = c(0, 55), type = "n", xlim = c(0.7,7),
       yaxt = "n", xaxt = "n", ylab = "", xlab = "", bty = "n", las = 1)
  abline(h = election_res[i])
  abline(h = apply(struct_forecast, 2, mean)[i]*100, lty = "dashed")
  segments(x0 = 7:1, y0 = df_forecast$low[df_forecast$x==sel], y1 = df_forecast$high[df_forecast$x==sel], col = adjustcolor("grey", alpha = 0.8), lwd = 6, lend = 1)
  segments(x0 = 7:1, y0 = df_forecast$low95[df_forecast$x==sel], y1 = df_forecast$high95[df_forecast$x==sel], col = adjustcolor("grey", alpha = 0.6), lwd = 6, lend = 1)
  
  points(x = 7:1, y = avg_polls[,i], col = "black")
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


# RMSE Comparison with montly poll averages in text


apply(sweep(avg_polls, 2, election_res)^2, 1, function(x) sqrt(mean(x)))




# Appendix B.3.1 Table 1

# Load Data
Elections <- c(2002, 2005, 2009, 2013, 2017)

load("../../data/ger/Structural/ger_model_df.RData")

election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

election_id <- election_years_df$election_id[election_years_df$year == min(Elections)]

# Predictors
predictors <- c("voteshare_l1", "chancellor_party", "polls_200_230")
dependent<- "voteshare"

# Subset to complete Cases 
which_cases <- complete.cases(ger_df_long[, c(predictors)])
dat <- ger_df_long[, c("party",dependent, predictors, "election")]

# Fill in 2017 election results

dat$voteshare[is.na(dat$voteshare)] <- c(12.6, 32.9, 10.7, 8.9, 9.2, 5, 20.5)


dat_sub <- dat[which_cases & dat$election >= election_id,]

# Get mean structural forecasts
structural_forecasts <- NULL



for(Election in Elections){
tmp <- readRDS(file = paste0("../../output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))
tmp <- apply(tmp, 2, mean)
structural_forecasts <- c(structural_forecasts, tmp)
}

rmse_dat <- data.frame(party = dat_sub$party, result = dat_sub$voteshare, 
                      struct_forecast = structural_forecasts*100,
                      election = dat_sub$election)

rmse_struct <- c(sqrt(mean((rmse_dat$struct_forecast - rmse_dat$result)^2)),
                 aggregate((rmse_dat$struct_forecast - rmse_dat$result)^2, 
                           list(factor(rmse_dat$election)), 
                           function(x) sqrt(mean(x)))$x)

cutoffs <- c(2, 8, 36, 64, 92, 116, 148)



rmse_table <- rmse_struct

for(cutoff in cutoffs){
combined_forecasts <- NULL
for(Election in Elections){
  tmp <- readRDS(file=paste0("../../output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))
  
  tmp <- tmp$forecast
  tmp <- apply(tmp, 2, mean)
  combined_forecasts <- c(combined_forecasts, tmp)
}

rmse_dat <- data.frame(party = dat_sub$party, result = dat_sub$voteshare, 
                       comb_forecast = combined_forecasts*100,
                       election = dat_sub$election)

tmp <- c(sqrt(mean((rmse_dat$comb_forecast - rmse_dat$result)^2)),
                 aggregate((rmse_dat$comb_forecast - rmse_dat$result)^2, 
                           list(factor(rmse_dat$election)), 
                           function(x) sqrt(mean(x)))$x)
rmse_table <- rbind(rmse_table, tmp)
}


colnames(rmse_table) <- c("2002-17", "2002", "2005", "2009", "2013", "2017")
rownames(rmse_table) <- c("Structural", paste(cutoffs))

rmse_table <- round(rmse_table, 2)

xtable::xtable(rmse_table)


# RMSE Comparison with monthly polling average

monthly <- NULL

for(Election in Elections){
  file <- list.files(path = "../../output/ger/draws/combined_model", 
                     pattern = paste0("draws*.*_",Election,"_",2,".RDS"), 
                     full.names = T)
  
  df <- readRDS(file)
  
  nParties <- length(df$party_names)
  
  df_polls <- df$polls
  
  
  monthly_polls <- matrix(NA, nrow = length(cutoffs), ncol = nParties)
  
  for(i in 1:length(cutoffs)){
    monthly_polls[i,] <- colMeans(df_polls[which(df_polls$days_to_election >= cutoffs[i] & df_polls$days_to_election <= (cutoffs[i]+35)),6:ncol(df_polls)])
  }
  
  colnames(monthly_polls) <- names(df_polls[6:ncol(df_polls)])
  rownames(monthly_polls) <- cutoffs
  
  monthly[[paste(Election)]] <- monthly_polls
  

}


comb_all_el <- NULL

for(i in 1:7){
  comb_all_el[i] <- sqrt(mean((do.call(c, lapply(monthly, `[`,i, )) - rmse_dat$result)^2))
}

  
monthly_rmse <- matrix(NA, nrow = 7, ncol = 5)

for(i in 1:5){
  monthly_rmse[,i] <- apply(sweep(monthly[[paste(Elections[i])]], 2,dat_sub[dat_sub$election==(i+14),"voteshare"])^2,1,function(x) sqrt(mean(x)))
  }

rmse_table_polls <- matrix(NA, nrow = 14, ncol = 6)

for(i in 1:7){
  rmse_table_polls[(i+(i-1)), ] <- rmse_table[i+1,]
  rmse_table_polls[(i+i), ] <- cbind(round(comb_all_el ,2), round(monthly_rmse, 2))[i,]
}


colnames(rmse_table_polls) <- c("2002-17", "2002", "2005", "2009", "2013", "2017")
rownames(rmse_table_polls) <- paste(c("Combined Model", "Avg. of Polls"), sort(rep(cutoffs,2)))

xtable::xtable(rmse_table_polls)

# B.3.1 Figure 4

Election <- 2017

struct_forecast <- readRDS(paste0("../../output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))


adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), colnames(struct_forecast))


struct_forecast <- struct_forecast[,adjustOrder]



cutoffs <- c(148, 64, 8, 2)

plot_df <- NULL

for(cutoff in cutoffs){

  file <- list.files(path = "../../output/draws/combined_model", 
                     pattern = paste0("draws*.*_",Election,"_",cutoff), 
                     full.names = T)
  
  df <- readRDS(file)
  
  party_names <- c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth")
  
  adjustOrder <- match(party_names, df$party_names)
  
  
  levels <- df$levels[,adjustOrder,]
  
  means <- apply(levels, c(2,3), mean)
  tmp <- data.frame(days_to = 366:1, t(means))
  colnames(tmp)[2:8] <- party_names
  
  
  tmp <- gather(tmp, party, value, cdu:oth, factor_key=TRUE)
  
  
  
  tmp_hi <- apply(levels,c(2,3), function(x) quantile(x,probs = 0.975))
  tmp_hi <- data.frame(days_to = 366:1, t(tmp_hi))
  
  tmp_hi <- gather(tmp_hi, party, high, X1:X7, factor_key=TRUE)
  
  tmp_lo <- apply(levels,c(2,3), function(x) quantile(x,probs = 0.025))
  tmp_lo <- data.frame(days_to = 366:1, t(tmp_lo))
  
  tmp_lo <- gather(tmp_lo, party, low, X1:X7, factor_key=TRUE)
  
  tmp$low <- tmp_lo$low
  tmp$high<- tmp_hi$high
  
  
  tmp_hi <- apply(levels,c(2,3), function(x) quantile(x,probs = 11/12))
  tmp_hi <- data.frame(days_to = 366:1, t(tmp_hi))
  
  tmp_hi <- gather(tmp_hi, party, high, X1:X7, factor_key=TRUE)
  
  tmp_lo <- apply(levels,c(2,3), function(x) quantile(x,probs = 1/12))
  tmp_lo <- data.frame(days_to = 366:1, t(tmp_lo))
  
  tmp_lo <- gather(tmp_lo, party, low, X1:X7, factor_key=TRUE)
  
  tmp$low56 <- tmp_lo$low
  tmp$high56 <- tmp_hi$high
  
  tmp[,3:7] <- tmp[,3:7]*100
  
  tmp$cutoff <- cutoff
  tmp
  plot_df <- rbind(plot_df, tmp)
}

round(plot_df$low[plot_df$party == "spd" & plot_df$cutoff == 148][366],1)
round(plot_df$high[plot_df$party == "spd" & plot_df$cutoff == 148][366],1)

round(plot_df$low[plot_df$party == "spd" & plot_df$cutoff == 8][366],1)
round(plot_df$high[plot_df$party == "spd" & plot_df$cutoff == 8][366],1)

pdf(file="../../output/ger/plots/combined_model/figure4.pdf", height=5, width=8, family="URWTimes")
par(mar=c(0,0,2,0)+.1, oma=c(4,4,0,6)+.1, mfrow=c(2,2))

sel <- "spd"

ax <- 1

for(cutoff in cutoffs){
plot(x = -plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff], 
     y = plot_df$value[plot_df$party == sel & plot_df$cutoff == cutoff], 
     type = "l", 
     ylim = c(15, 40),
     xlim = c(-201,-1),
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "",
     xlab = "",
     main = paste0("Polls ", cutoff, " days prior to election"))

points(x = -df$polls$days_to_election[df$polls$days_to_election>=cutoff],
       y = pull(df$polls, sel)[df$polls$days_to_election>=cutoff],
       pch = df$polls$iid,
       col = "grey")

polygon(x = c(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff],
              rev(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff])),
        
        y = c(plot_df$low56[plot_df$party == sel & plot_df$cutoff == cutoff],
              rev(plot_df$high56[plot_df$party == sel & plot_df$cutoff == cutoff])),
        
        col = rgb(.8, .8, .8, .3),
        border = NA)

polygon(x = c(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff],
              rev(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff])),
        
        y = c(plot_df$low[plot_df$party == sel & plot_df$cutoff == cutoff],
              rev(plot_df$high[plot_df$party == sel & plot_df$cutoff == cutoff])),
        
        col = rgb(.8, .8, .8, .3),
        border = NA)

lines(x = -plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff], 
      y = plot_df$value[plot_df$party == sel & plot_df$cutoff == cutoff])

abline(h = seq(15,40,5), lty = "dashed", 
       col = adjustcolor("lightgrey", alpha = 0.5))

abline(v = c(-200,-148,-64,-8,-2), lty = "dashed", 
       col = adjustcolor("lightgrey", alpha = 0.5))

abline(h = 20.5, 
       col = "black", lwd = 1.5)

abline(h = mean(struct_forecast[,sel])* 100,  
       col = "black", lwd = 1.5, lty = "dashed")

if(ax %in% c(1,3)) {
 axis(2, at = seq(15,40,5), labels = seq(15,40,5),las = 1)
}
if(ax %in% c(3,4)) {
 axis(1, -plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff], 
      at = c(-200,-148,-64,-8,-2), labels = c(NA,148,64,8,2))
}
ax <- ax + 1
}

mtext("Days until election", 1, 2, outer=TRUE)
mtext("Vote Share (%) for SPD", 2, 2, outer=TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = levels(as.factor(df$polls$institute)), xpd = TRUE, horiz = F, 
       inset = c(0,0), bty = "n", pch = 1:7, cex = 0.7)

dev.off()

# Other Model Comparisons

# Appendix B.3.1 Table 3

# Load Data
Elections <- c(2017)

load("../../data/ger/Structural/ger_model_df.RData")

election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

election_id <- election_years_df$election_id[election_years_df$year == min(Elections)]

# Predictors
predictors <- c("voteshare_l1", "chancellor_party", "polls_200_230")
dependent<- "voteshare"

# Subset to complete Cases 
which_cases <- complete.cases(ger_df_long[, c(predictors)])
dat <- ger_df_long[, c("party",dependent, predictors, "election")]

# Fill in 2017 election results

dat$voteshare[is.na(dat$voteshare)] <- c(12.6, 32.9, 10.7, 8.9, 9.2, 5, 20.5)


dat_sub <- dat[which_cases & dat$election >= election_id,]

# Get mean structural forecasts
structural_forecasts <- NULL
model <- NULL


for(Election in Elections){
  tmp <- readRDS(file = paste0("../../output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))
  tmp <- apply(tmp, 2, mean)
  structural_forecasts <- c(structural_forecasts, tmp)
  model <- c(model, rep(1, length(tmp)))
  tmp <- readRDS(file = paste0("../../output/ger/forecasts/structural/", Election, "_structural_forecast_only_l1.RDS"))
  tmp <- apply(tmp, 2, mean)
  structural_forecasts <- c(structural_forecasts, tmp)
  model <- c(model, rep(2, length(tmp)))
  tmp <- readRDS(file = paste0("../../output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))
  tmp <- apply(tmp, 2, mean)
  structural_forecasts <- c(structural_forecasts, tmp)
  model <- c(model, rep(3, length(tmp)))
}

rmse_dat <- data.frame(party = dat_sub$party, result = dat_sub$voteshare, 
                       struct_forecast = structural_forecasts*100,
                       model = model)

rmse_struct <- c(sqrt(mean((rmse_dat$struct_forecast - rmse_dat$result)^2)),
                 aggregate((rmse_dat$struct_forecast - rmse_dat$result)^2, 
                           list(factor(rmse_dat$model)), 
                           function(x) sqrt(mean(x)))$x)

cutoffs <- c(2, 8, 36, 64, 92, 116, 148)



rmse_table <- rmse_struct

for(cutoff in cutoffs){
  combined_forecasts <- NULL
  model <- NULL
  for(Election in Elections){
    tmp <- readRDS(file=paste0("../../output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))
    
    tmp <- tmp$forecast
    tmp <- apply(tmp, 2, mean)
    combined_forecasts <- c(combined_forecasts, tmp)
    model <- c(model, rep(1, length(tmp)))
    
    tmp <- readRDS(file=paste0("../../output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,"_only_l1.RDS"))
    
    tmp <- tmp$forecast
    tmp <- apply(tmp, 2, mean)
    combined_forecasts <- c(combined_forecasts, tmp)
    model <- c(model, rep(2, length(tmp)))
    
    tmp <- readRDS(file=paste0("../../output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,"_nohouse.RDS"))
    
    tmp <- tmp$forecast
    tmp <- apply(tmp, 2, mean)
    combined_forecasts <- c(combined_forecasts, tmp)
    model <- c(model, rep(3, length(tmp)))
  }
  
  rmse_dat <- data.frame(party = dat_sub$party, result = dat_sub$voteshare, 
                         comb_forecast = combined_forecasts*100,
                         model = model)
  
  tmp <- c(sqrt(mean((rmse_dat$comb_forecast - rmse_dat$result)^2)),
           aggregate((rmse_dat$comb_forecast - rmse_dat$result)^2, 
                     list(factor(rmse_dat$model)), 
                     function(x) sqrt(mean(x)))$x)
  rmse_table <- rbind(rmse_table, tmp)
}


colnames(rmse_table) <- c("avg", "Full Fundamentals Model", "Only last vote share Fundamentals Model", "No House")
rownames(rmse_table) <- c("Structural", paste(cutoffs))

rmse_table <- round(rmse_table, 2)

xtable::xtable(rmse_table)



# Estimated Correlation Matrix for Evolution Variance

Election <- 2017
cutoff <- 2
tmp <- readRDS(file=paste0("../../output/ger/draws/combined_model/res_brw_",Election,"_",cutoff,".RDS"))

tmp1 <- readRDS(file=paste0("../../output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))

tmp1 <- tmp1$party_names
tmp <- as.matrix(tmp)


S_m <- round(apply(tmp[,grep("^S_cor", colnames(tmp))],2, mean) %>% matrix(ncol = 6, nrow = 6) ,2)
S_95 <- paste0("(",round(apply(tmp[,grep("^S_cor", colnames(tmp))],2, quantile, 0.025) %>% matrix(ncol = 6, nrow = 6) ,2), 
               "; ",
               round(apply(tmp[,grep("^S_cor", colnames(tmp))],2, quantile, 0.975) %>% matrix(ncol = 6, nrow = 6) ,2), ")") %>%  matrix(ncol = 6, nrow = 6)

S_table <- NULL
for(i in 1:6){
  S_table <- rbind(S_table, S_m[i,], S_95[i,])
}

colnames(S_table) <- tmp1[1:6]

rownames(S_table) <- rep(c("mean", "95 % CI"),6)

xtable::xtable(S_table)

############################
# Figures for New Zealand
############################


# Figure C.1.1 1

df <- readRDS(file="../../output/nz/draws/combined_model/draws_forcast_levels_2017_2.RDS")

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

# Numbers in text

df_forecast

round(sqrt(mean((round(df_forecast,1)[,1] -  c(44.45, 36.89, 6.27, 7.2, 5.19))^2)),2)




df_forecast$name <- plot_names
df_forecast$name_eng <- plot_names

df_forecast$y <- df_forecast$value
df_forecast$x <- seq(0, 5, 1)


pdf(file="../../output/nz/plots/combined_model/nz_figure1.pdf",  height=5, width=8, family="URWTimes")
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

# segments(y0 = c(33.7,  39.3), y1 = c(35.7, 39.3), x0 = c(0.5,  0.5), x1 = c(1, 0.9), lty = "dashed",  col = adjustcolor("black", alpha = 0.7))
# segments(y0 = c(30.9), y1 = c(32.9), x0 = c(1.35), x1 = c(1.15), lty = "dashed",  col = adjustcolor("black", alpha = 0.4))

segments(x0 = c(1,2,3,4,5), y0 = 0, y1 = c(44.45, 36.89, 6.27, 7.2, 5.19), lwd = 35, col = adjustcolor("grey", alpha = 0.3), lend = 1)
#segments(x0 = c(1,2,3,4,5,6,7), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.5), lend = 1)
segments(x0 = c(1,2,3,4,5), y0 = df_forecast$low95, y1 = df_forecast$high95, lwd = 20, col = adjustcolor("grey", alpha = 0.6), lend = 1)
segments(x0 = c(1,2,3,4,5), y0 = df_forecast$low, y1 = df_forecast$high, lwd = 20, col = adjustcolor("grey", alpha = 0.99), lend = 1)

points(x = c(1,2,3,4,5),
       y = df_forecast$value, col = "white", lwd = 2)
text(y = df_forecast$value, x = c(1,2,3,4,5)-0.25, labels = df_forecast$value, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
text(y = c(44.45, 36.89, 6.27, 7.2, 5.19), x = c(1,2,3,4,5)+0.3, labels = c(44.45, 36.89, 6.27, 7.2, 5.19), cex = 0.9, col = adjustcolor("black", alpha = 0.4))
axis(1, at = c(1,2,3,4,5), labels = df_forecast$name_eng, las = 1, tick = 0, cex.axis = 1.2 )
axis(2, at = c(5,10,20,30,40,50), labels = c(5,10,20,30,40,50), las = 1, tick = 0, cex.axis = 1.2)
mtext("Vote Share (%)", side=2, line=3, cex=1.2)

# text(y = c(33.7,  39.3), x = 1-1, labels = c("Prediction",  "5/6 CI"), adj = 0, cex = 0.9,  col = adjustcolor("black", alpha = 0.7))
# text(y = c(30.9), x = 1+0.4, labels = c( "Result"), adj = 0, cex = 0.9,  col = adjustcolor("black", alpha = 0.4))
dev.off()



# Quantities of Interest in text

# Probability of National becoming strongest party
round(sum(t(apply(-forecast[,1:5], 1, rank))[,1] == 1) / nrow(forecast) * 100, 1)

# Probability for Greens to clear the 5 percent threshold
round(sum(forecast[,3] >= 0.05)  / nrow(forecast) * 100, 1)

# Probability for NZ First to clear the 5 percent threshold
round(sum(forecast[,4] >= 0.05)  / nrow(forecast) * 100, 1)

# C.1 Figure 5

# Figure 2

Election <- 2017


cutoffs <- c(2, 8, 36, 64, 92, 116, 148)

df_forecast <- NULL

for(cutoff in cutoffs){
  
  file <- list.files(path = "../../output/nz/draws/combined_model", 
                     pattern = paste0("draws*.*_",Election,"_",cutoff), 
                     full.names = T)
  
  df <- readRDS(file)
  
  
  adjustOrder <- match(c("nat", "lab", "gre", "nzf", "oth"), df$party_names)
  
  
  forecast <- df$forecast[,adjustOrder]
  
  tmp_forecast <- data.frame(y = apply(forecast, 2, mean),
                             ci = t(apply(forecast, 2, function(x) quantile(x, c(1/12, 11/12)))),
                             ci95 = t(apply(forecast, 2, function(x) quantile(x, c(0.025, 0.975)))))
  
  
  
  rownames(tmp_forecast) <- plot_names
  colnames(tmp_forecast) <- c("value", "low", "high", "low95", "high95")
  
  tmp_forecast <- round(tmp_forecast*100, 1)
  
  tmp_forecast$name <- c("National", "Labour", "Green", "NZ First", "Others")
  tmp_forecast$name_eng <- c("National", "Labour", "Green", "NZ First", "Others")
  tmp_forecast$t <- cutoff

  tmp_forecast$y <- tmp_forecast$value
  tmp_forecast$x <- seq(0, 4 ,1)
  
  df_forecast <- rbind(df_forecast, tmp_forecast)
}

# Get polling averages

file <- list.files(path = "../../output/nz/draws/combined_model", 
                   pattern = paste0("draws*.*_",2017,"_",2), 
                   full.names = T)

df <- readRDS(file)

avg_polls <- matrix(NA, nrow = length(cutoffs), ncol = 5)
df_polls <- df$polls
for(i in 1:length(cutoffs)){
  avg_polls[i,] <- apply(df_polls[(df_polls$days_to_election >= cutoffs[i] & df_polls$days_to_election < cutoffs[i]+35),][,6:ncol(df_polls)],2,mean)
}

colnames(avg_polls) <- names(df_polls[6:ncol(df_polls)])
rownames(avg_polls) <- cutoffs

adjustOrder <- match(c("nat", "lab", "gre", "nzf", "oth"), colnames(avg_polls))

avg_polls <- avg_polls[,adjustOrder]

struct_forecast <- readRDS(paste0("../../output/nz/forecasts/structural_pre_train/", Election, "_structural_forecast.RDS"))


adjustOrder <- match(c("nat", "lab", "gre", "nzf", "oth"), colnames(struct_forecast))


struct_forecast <- struct_forecast[,adjustOrder]

parties <- unique(df_forecast$x)

election_res <- c(44.45, 36.89, 6.27, 7.2, 5.19)


pdf(file="../../output/nz/plots/combined_model/nz_figure2.pdf", height=5, width=8, family="URWTimes")
par(mar=c(0,0,2,0)+.1)
par(oma=c(5,5,0,0)+.1)
layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE))

for(i in 1:4){
  sel <- parties[i]
  # ymin <- floor(min(df_forecast$low[df_forecast$x==sel])-1/4*min(df_forecast$low[df_forecast$x==sel]))
  # ymax <- ceiling(max(df_forecast$high[df_forecast$x==sel])+1/4*max(df_forecast$high[df_forecast$x==sel]))
  plot(x = 7:1, y = df_forecast$value[df_forecast$x==sel], 
       ylim = c(0, 75), type = "n", xlim = c(0.7,7),
       yaxt = "n", xaxt = "n", ylab = "", xlab = "", bty = "n", las = 1)
  abline(h = election_res[i])
  abline(h = apply(struct_forecast, 2, mean)[i]*100, lty = "dashed")
  segments(x0 = 7:1, y0 = df_forecast$low[df_forecast$x==sel], y1 = df_forecast$high[df_forecast$x==sel], col = adjustcolor("grey", alpha = 0.8), lwd = 6, lend = 1)
  segments(x0 = 7:1, y0 = df_forecast$low95[df_forecast$x==sel], y1 = df_forecast$high95[df_forecast$x==sel], col = adjustcolor("grey", alpha = 0.6), lwd = 6, lend = 1)
  
  points(x = 7:1, y = avg_polls[,i], col = "black")
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



# C.3 Figure 8

Election <- 2017

struct_forecast <- readRDS(paste0("../../output/nz/forecasts/structural_pre_train/", Election, "_structural_forecast.RDS"))


adjustOrder <- match(c("nat", "lab", "gre", "nzf", "oth"), colnames(struct_forecast))


struct_forecast <- struct_forecast[,adjustOrder]



cutoffs <- c(148, 64, 8, 2)
plot_df <- NULL

for(cutoff in cutoffs){
  
  file <- list.files(path = "../../output/nz/draws/combined_model", 
                     pattern = paste0("draws*.*_",Election,"_",cutoff), 
                     full.names = T)
  
  df <- readRDS(file)
  
  party_names <- c("nat", "lab", "gre", "nzf", "oth")
  
  adjustOrder <- match(party_names, df$party_names)
  
  
  levels <- df$levels[,adjustOrder,]
  
  means <- apply(levels, c(2,3), mean)
  tmp <- data.frame(days_to = 366:1, t(means))
  colnames(tmp)[2:6] <- party_names
  
  
  tmp <- gather(tmp, party, value, nat:oth, factor_key=TRUE)
  
  
  
  tmp_hi <- apply(levels,c(2,3), function(x) quantile(x,probs = 0.975))
  tmp_hi <- data.frame(days_to = 366:1, t(tmp_hi))
  
  tmp_hi <- gather(tmp_hi, party, high, X1:X5, factor_key=TRUE)
  
  tmp_lo <- apply(levels,c(2,3), function(x) quantile(x,probs = 0.025))
  tmp_lo <- data.frame(days_to = 366:1, t(tmp_lo))
  
  tmp_lo <- gather(tmp_lo, party, low, X1:X5, factor_key=TRUE)
  
  tmp$low <- tmp_lo$low
  tmp$high<- tmp_hi$high
  
  
  tmp_hi <- apply(levels,c(2,3), function(x) quantile(x,probs = 11/12))
  tmp_hi <- data.frame(days_to = 366:1, t(tmp_hi))
  
  tmp_hi <- gather(tmp_hi, party, high, X1:X5, factor_key=TRUE)
  
  tmp_lo <- apply(levels,c(2,3), function(x) quantile(x,probs = 1/12))
  tmp_lo <- data.frame(days_to = 366:1, t(tmp_lo))
  
  tmp_lo <- gather(tmp_lo, party, low, X1:X5, factor_key=TRUE)
  
  tmp$low56 <- tmp_lo$low
  tmp$high56 <- tmp_hi$high
  
  tmp[,3:7] <- tmp[,3:7]*100
  
  tmp$cutoff <- cutoff
 
  plot_df <- rbind(plot_df, tmp)
}

round(plot_df$low[plot_df$party == "lab" & plot_df$cutoff == 148][366],1)
round(plot_df$high[plot_df$party == "lab" & plot_df$cutoff == 148][366],1)

round(plot_df$low[plot_df$party == "lab" & plot_df$cutoff == 8][366],1)
round(plot_df$high[plot_df$party == "lab" & plot_df$cutoff == 8][366],1)

pdf(file="../../output/nz/plots/combined_model/nz_figure5.pdf", height=5, width=8, family="URWTimes")
par(mar=c(0,0,2,0)+.1, oma=c(4,4,0,6)+.1, mfrow=c(2,2))

sel <- "lab"

ax <- 1

for(cutoff in cutoffs){
  plot(x = -plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff], 
       y = plot_df$value[plot_df$party == sel & plot_df$cutoff == cutoff], 
       type = "l", 
       ylim = c(5, 50),
       xlim = c(-201,-1),
       bty = "n",
       yaxt = "n",
       xaxt = "n",
       ylab = "",
       xlab = "",
       main = paste0("Polls ", cutoff, " days prior to election"))
  
  points(x = -df$polls$days_to_election[df$polls$days_to_election>=cutoff],
         y = pull(df$polls, sel)[df$polls$days_to_election>=cutoff],
         pch = df$polls$iid,
         col = "grey")
  
  polygon(x = c(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff],
                rev(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff])),
          
          y = c(plot_df$low56[plot_df$party == sel & plot_df$cutoff == cutoff],
                rev(plot_df$high56[plot_df$party == sel & plot_df$cutoff == cutoff])),
          
          col = rgb(.8, .8, .8, .3),
          border = NA)
  
  polygon(x = c(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff],
                rev(-plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff])),
          
          y = c(plot_df$low[plot_df$party == sel & plot_df$cutoff == cutoff],
                rev(plot_df$high[plot_df$party == sel & plot_df$cutoff == cutoff])),
          
          col = rgb(.8, .8, .8, .3),
          border = NA)
  
  lines(x = -plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff], 
        y = plot_df$value[plot_df$party == sel & plot_df$cutoff == cutoff])
  
  abline(h = seq(5,50,5), lty = "dashed", 
         col = adjustcolor("lightgrey", alpha = 0.5))
  
  abline(v = c(-200,-148,-64,-8,-2), lty = "dashed", 
         col = adjustcolor("lightgrey", alpha = 0.5))
  
  abline(h = 36.89, 
         col = "black", lwd = 1.5)
  
  abline(h = mean(struct_forecast[,sel])* 100,  
         col = "black", lwd = 1.5, lty = "dashed")
  
  if(ax %in% c(1,3)) {
    axis(2, at = seq(5,50,5), labels = seq(5,50,5),las = 1)
  }
  if(ax %in% c(3,4)) {
    axis(1, -plot_df$days_to[plot_df$party == sel & plot_df$cutoff == cutoff], 
         at = c(-200,-148,-64,-8,-2), labels = c(NA,148,64,8,2))
  }
  ax <- ax + 1
}

mtext("Days until election", 1, 2, outer=TRUE)
mtext("Vote Share (%) for Labour", 2, 2, outer=TRUE)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = levels(as.factor(df$polls$institute)), xpd = TRUE, horiz = F, 
       inset = c(0,0), bty = "n", pch = 1:7, cex = 0.7)

dev.off()



# RMSE Table for New Zealand

# Load Data
Elections <- c(2011, 2014, 2017)

nz_df_long <- readRDS("../../data/nz/Structural/nz_structural.RDS")

election_years <- unique(nz_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

election_id <- election_years_df$election_id[election_years_df$year == min(Elections)]

# Predictors
predictors <- c("voteshare_l1", "chancellor_party", "polls_200_230")
dependent<- "voteshare"

# Subset to complete Cases 
which_cases <- complete.cases(nz_df_long[, c(predictors)])
dat <- nz_df_long[, c("party",dependent, predictors, "election")]

# Fill in 2017 election results

dat$voteshare[is.na(dat$voteshare)] <- c(44.45, 36.89, 6.27, 7.2, 5.19)


dat_sub <- dat[which_cases & dat$election >= election_id,]

# Get mean structural forecasts
structural_forecasts <- NULL



for(Election in Elections){
  tmp <- readRDS(file = paste0("../../output/nz/forecasts/structural_pre_train/", Election, "_structural_forecast.RDS"))
  tmp <- apply(tmp, 2, mean)
  structural_forecasts <- c(structural_forecasts, tmp)
}

rmse_dat <- data.frame(party = dat_sub$party, result = dat_sub$voteshare, 
                       struct_forecast = structural_forecasts*100,
                       election = dat_sub$election)

rmse_struct <- c(sqrt(mean((rmse_dat$struct_forecast - rmse_dat$result)^2)),
                 aggregate((rmse_dat$struct_forecast - rmse_dat$result)^2, 
                           list(factor(rmse_dat$election)), 
                           function(x) sqrt(mean(x)))$x)

cutoffs <- c(2, 8, 36, 64, 92, 116, 148)



rmse_table <- rmse_struct

for(cutoff in cutoffs){
  combined_forecasts <- NULL
  for(Election in Elections){
    tmp <- readRDS(file=paste0("../../output/nz/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))
    
    tmp <- tmp$forecast
    tmp <- apply(tmp, 2, mean)
    combined_forecasts <- c(combined_forecasts, tmp)
  }
  
  rmse_dat <- data.frame(party = dat_sub$party, result = dat_sub$voteshare, 
                         comb_forecast = combined_forecasts*100,
                         election = dat_sub$election)
  
  tmp <- c(sqrt(mean((rmse_dat$comb_forecast - rmse_dat$result)^2)),
           aggregate((rmse_dat$comb_forecast - rmse_dat$result)^2, 
                     list(factor(rmse_dat$election)), 
                     function(x) sqrt(mean(x)))$x)
  rmse_table <- rbind(rmse_table, tmp)
}


colnames(rmse_table) <- c("2011-17", "2011", "2014", "2017")
rownames(rmse_table) <- c("Structural", paste(cutoffs))

rmse_table <- round(rmse_table, 2)

xtable::xtable(rmse_table)


# RMSE Comparison with monthly polling average

monthly <- NULL

for(Election in Elections){
  file <- list.files(path = "../../output/nz/draws/combined_model", 
                     pattern = paste0("draws*.*_",Election,"_",2,".RDS"), 
                     full.names = T)
  
  df <- readRDS(file)
  
  nParties <- length(df$party_names)
  
  df_polls <- df$polls
  
  
  monthly_polls <- matrix(NA, nrow = length(cutoffs), ncol = nParties)
  
  for(i in 1:length(cutoffs)){
    monthly_polls[i,] <- colMeans(df_polls[which(df_polls$days_to_election >= cutoffs[i] & df_polls$days_to_election <= (cutoffs[i]+35)),6:ncol(df_polls)])
  }
  
  colnames(monthly_polls) <- names(df_polls[6:ncol(df_polls)])
  rownames(monthly_polls) <- cutoffs
  
  monthly[[paste(Election)]] <- monthly_polls
  
  
}

comb_all_el <- NULL

for(i in 1:7){
  comb_all_el[i] <- sqrt(mean((do.call(c, lapply(monthly, `[`,i, )) - rmse_dat$result)^2))
}

monthly_rmse <- matrix(NA, nrow = 7, ncol = 3)

for(i in 1:3){
  monthly_rmse[,i] <- apply(sweep(monthly[[paste(Elections[i])]], 2,dat_sub[dat_sub$election==(i+5),"voteshare"])^2,1,function(x) sqrt(mean(x)))
}

rmse_table_polls <- matrix(NA, nrow = 14, ncol = 4)

for(i in 1:7){
  rmse_table_polls[(i+(i-1)), ] <- rmse_table[i+1,]
  rmse_table_polls[(i+i), ] <- cbind(round(comb_all_el,2), round(monthly_rmse, 2))[i,]
}



colnames(rmse_table_polls) <- c("2011-17", "2011", "2014", "2017")
rownames(rmse_table_polls) <- paste(c("Combined Model", "Avg. of Polls"), sort(rep(cutoffs,2)))

xtable::xtable(rmse_table_polls)


