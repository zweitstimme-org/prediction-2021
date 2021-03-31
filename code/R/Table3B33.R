# RMSE Comparison with monthly polling average

# Load Data
Elections <- c(2002, 2005, 2009, 2013, 2017)

load("data/ger/Structural/ger_model_df.RData")

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
  tmp <- readRDS(file = paste0("output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))
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
    tmp <- readRDS(file=paste0("output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))
    
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

monthly <- NULL

for(Election in Elections){
  file <- list.files(path = "output/ger/draws/combined_model", 
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

print(xtable(rmse_table_polls), file = "output/paper/appendix_table3.tex")