# Load Data
Elections <- c(2017)

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
model <- NULL


for(Election in Elections){
  tmp <- readRDS(file = paste0("output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))
  tmp <- apply(tmp, 2, mean)
  structural_forecasts <- c(structural_forecasts, tmp)
  model <- c(model, rep(1, length(tmp)))
  tmp <- readRDS(file = paste0("output/ger/forecasts/structural/", Election, "_structural_forecast_only_l1.RDS"))
  tmp <- apply(tmp, 2, mean)
  structural_forecasts <- c(structural_forecasts, tmp)
  model <- c(model, rep(2, length(tmp)))
  tmp <- readRDS(file = paste0("output/ger/forecasts/structural/", Election, "_structural_forecast.RDS"))
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
    tmp <- readRDS(file=paste0("output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))
    
    tmp <- tmp$forecast
    tmp <- apply(tmp, 2, mean)
    combined_forecasts <- c(combined_forecasts, tmp)
    model <- c(model, rep(1, length(tmp)))
    
    tmp <- readRDS(file=paste0("output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,"_only_l1.RDS"))
    
    tmp <- tmp$forecast
    tmp <- apply(tmp, 2, mean)
    combined_forecasts <- c(combined_forecasts, tmp)
    model <- c(model, rep(2, length(tmp)))
    
    tmp <- readRDS(file=paste0("output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,"_nohouse.RDS"))
    
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

rmse_table <- rmse_table[,2:4]

rmse_table <- round(rmse_table, 2)

print(xtable(rmse_table), file = "output/paper/appendix_table2.tex")