Election <- 2017


cutoffs <- c(2, 8, 36, 64, 92, 116, 148)

df_forecast <- NULL

for(cutoff in cutoffs){
  
  file <- list.files(path = "output/nz/draws/combined_model", 
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

file <- list.files(path = "output/nz/draws/combined_model", 
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

struct_forecast <- readRDS(paste0("output/nz/forecasts/structural_pre_train/", Election, "_structural_forecast.RDS"))


adjustOrder <- match(c("nat", "lab", "gre", "nzf", "oth"), colnames(struct_forecast))


struct_forecast <- struct_forecast[,adjustOrder]

parties <- unique(df_forecast$x)

election_res <- c(44.45, 36.89, 6.27, 7.2, 5.19)

pdf("output/paper/appendix_figure5.pdf", width = 12, height = 8)
par(mar=c(0,0,2,0)+.1)
par(oma=c(5,5,0,0)+.1)
layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE))

for(i in 1:4){
  sel <- parties[i]

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
  
  axis(1, at = 7:1 , labels = paste(cutoffs), col = NA, col.ticks = 1, las = 2)

  if(i==1) axis(2, col = NA, col.ticks = 1, las = 1, cex.axis = 1.2)
  if(i>1) axis(2, col = "grey", col.ticks = NA, las = 1, labels = NA)
}

mtext("Days until election", 1, 3, outer=TRUE, cex = 1.2)
mtext("Vote Share (%)", 2, 3, outer=TRUE, cex = 1.2)
dev.off()