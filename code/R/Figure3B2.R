Election <- 2017

struct_forecast <- readRDS(paste0("output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))


adjustOrder <- match(c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"), colnames(struct_forecast))


struct_forecast <- struct_forecast[,adjustOrder]



cutoffs <- c(148, 64, 8, 2)

plot_df <- NULL

for(cutoff in cutoffs){
  
  file <- list.files(path = "output/ger/draws/combined_model", 
                     pattern = paste0("draws*.*_",Election,"_",cutoff,".RDS"), 
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