# RMSE Table for New Zealand

# Load Data
Elections <- c(2011, 2014, 2017)

nz_df_long <- readRDS("data/nz/Structural/nz_structural.RDS")

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
  tmp <- readRDS(file = paste0("output/nz/forecasts/structural_pre_train/", Election, "_structural_forecast.RDS"))
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
    tmp <- readRDS(file=paste0("output/nz/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))
    
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

print(xtable(rmse_table), file = "output/paper/appendix_table5.tex")