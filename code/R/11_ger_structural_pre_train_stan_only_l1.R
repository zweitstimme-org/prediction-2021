### ----------------------------------------------------------
### Election polling forecasting 
### implementation of backward random walk for multi-party set-ups
### 
### Lukas Stoetzer & Marcel Neunhoeffer
### 
### 1. Set-up and Estimate Structural Model
###
### ----------------------------------------------------------

source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions



### 0. Pre-train Structual Model

# Specifications

upcoming_election <- 2017 # What's the next election?

# Parameters for Sampler
nIter <- 1000
nChains <- 6


model_file <- "../model_code/structural_pre_train.stan"

Elections <- c( 2017)

for(Election in Elections){



### Data for Structural Model

# Load Data
load("../../data/ger/Structural/ger_model_df.RData")

election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

election_id <- election_years_df$election_id[election_years_df$year == Election]

# Predictors
predictors <- c("voteshare_l1")
dependent<- "voteshare"

# Subset to complete Cases 
which_cases <- complete.cases(ger_df_long[, c(predictors)])
dat <- ger_df_long[, c("party",dependent, predictors, "election")]
dat_sub <- dat[which_cases & dat$election <= election_id,]

dat_sub$voteshare[dat_sub$election == election_id] <- NA


# Election results in a n x 1 matrix
election_res <- as.matrix(dat_sub[, dependent])

# Predictor for past elections in a n x k matrix
election_pred <- as.matrix(dat_sub[, predictors])
election_pred[,c(1)] <- election_pred[,c(1)] / 100

party_names <- dat_sub$party[is.na(election_res)]
nParties <- length(party_names) # Number of parties in upcoming election

nParties_vec <- as.vector(table(dat_sub$election)) # Number of parties in all elections

ii_obs <- which(complete.cases(c(election_res / 100))) # Indicator for observed elections for stan
ii_mis <- which(!complete.cases(c(election_res / 100))) # Indicator for missing elections

# Data in list for stan
forstan <- list(
  
  # Fundamental Model
  LA = length(unique(dat_sub$election)),
  L = length(unique(dat_sub$election)) + 1,# Number of elections
  N = length(election_res), #Number of observations
  Nobs = sum(complete.cases(c(election_res / 100))),
  Nmis = sum(!complete.cases(c(election_res / 100))),
  y_obs = c(election_res / 100)[ii_obs],  # Dependent variable
  x = election_pred, # Predictors past elections
  K = ncol(election_pred),   # Number of predictors
  election = dat_sub$election,
  nParties = nParties,
  ii_obs = ii_obs,
  ii_mis = ii_mis,
  s = nParties_vec
)


results <- stan(file = model_file, data = forstan,
            iter = nIter, chains = nChains, thin = 1, control = list(adapt_delta = 0.99, max_treedepth = 15))


saveRDS(results, file = paste0("../../output/ger/draws/structural_pre_train/", Election, "_structural_pre_train_stan_only_l1.RDS"))

res <- as.matrix(results)


jags_matrix <- as.matrix(res)

# What is this?
# 1/apply(jags_matrix,2, var)[grepl("b0\\[",names(apply(jags_matrix,2, var)))]
# 1/apply(jags_matrix,2, var)[grepl("b\\[",names(apply(jags_matrix,2, var)))]


structural_forecast <- jags_matrix[,grepl("y_mis\\[",colnames(jags_matrix))]
colnames(structural_forecast) <- dat_sub$party[!complete.cases(dat_sub$voteshare)]

print(apply(structural_forecast,2,mean))

saveRDS(structural_forecast, file = paste0("../../output/ger/forecasts/structural/", Election, "_structural_forecast_only_l1.RDS"))

apply(jags_matrix,2, mean)[grepl("y_mis\\[",names(apply(jags_matrix,2, mean)))]
apply(jags_matrix,2, quantile, c(1/12,11/12))[,grepl("y_mis\\[",names(apply(jags_matrix,2, mean)))]
jags_summary_df <- jags_summary(jags_matrix)


b_mean <- filter(jags_summary_df, str_detect(var, "^b\\[")) %>% magrittr::extract2("mean")

b_mean <- matrix(b_mean, ncol = 1)


b_0_mean <- filter(jags_summary_df, str_detect(var, "^b0\\[")) %>% magrittr::extract2("mean")


structural_inits <- list(b = b_mean, b0 = b_0_mean)

saveRDS(structural_inits, paste0("../../data/ger/structural_inits/", Election,"_structural_inits_only_l1.RDS"))

}
