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


model_file <- "../model_code/ger_combined_model_nohouse.stan"

Elections <- c(2017)


for(Election in Elections){

structural_inits <- readRDS(paste0("../../data/ger/structural_inits/", Election, "_structural_inits.RDS"))

initlist <- replicate(nChains, structural_inits, simplify=FALSE)



#### Data for Structural Model

# Load Data
load("../../data/ger/Structural/ger_model_df.RData")

election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

election_id <- election_years_df$election_id[election_years_df$year == Election]

# Predictors
predictors <- c("voteshare_l1", "chancellor_party", "polls_200_230")
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
election_pred[,c(1, 3)] <- election_pred[,c(1, 3)] / 100

party_names <- dat_sub$party[is.na(election_res)]
nParties <- length(party_names) # Number of parties in upcoming election

nParties_vec <- as.vector(table(dat_sub$election)) # Number of parties in all elections

ii_obs <- which(complete.cases(c(election_res / 100))) # Indicator for observed elections for stan
ii_mis <- which(!complete.cases(c(election_res / 100))) # Indicator for missing elections

# Predictors for upcoming election
election_pred_E <- dat[dat$election==election_id,c(predictors)]
rownames(election_pred_E) <- dat[dat$election==election_id,"party"]
election_pred_E <- election_pred_E[party_names,]
election_pred_E[,c(1, 3)] <- election_pred_E[,c(1, 3)] / 100

#### Poll Data for Dynamic Model

# Load Poll Data-base
ger_polls <- read_dta("../../data/ger/Polls/polls_btw.dta") %>% 
  filter(election==Election) %>% # Filter Polls for Election
  mutate(party = as.character(as_factor(party))) 

ger_polls$support[is.na(ger_polls$support)] <- 0

# Cutoffs (until which Date polls should be included)
cutoffs <- c(2, 8, 148, 116, 92, 64, 36)

for(cutoff in cutoffs){
  
  # Set time_window for data 
  max_days_to_election <- 365 # Start point up from which Polls are included
  time_window <- max_days_to_election:cutoff  # time window
  
  
  
  # Prepare Data
  polls <- ger_polls %>%
    filter(days_to_election %in% time_window) %>% # Filter Polls in time-window
    select(party, support, days_to_election, sample_size, institute) %>% # Select important information
    mutate(t= time_window[1] - days_to_election +1 ) %>% # create t variable from 1:366
    arrange(desc(days_to_election)) %>% # Arrange according to days to election
    mutate(iid = match(institute , sort(unique(institute)))) %>% # Create Institute id
    spread(party, support)  %>% 
    mutate(sample_size = replace(sample_size, is.na(sample_size), 1000)) %>%
    select(days_to_election, sample_size, institute,t,iid,party_names) %>% # Reshape to wide format
    na.omit() # Omit missing

  # Prepare Data for Jags
  Y <- round(as.matrix(polls[,party_names] / 100) * 
               polls$sample_size) # Grep existing Parties and transform share to number
 
  
  
  
forstan <- list(
  
  # Dynamic Model
  y = Y,
  nParties = nParties,
  nPeriods =  time_window[1]+1, 
  nPolls = nrow(Y),
  iid = polls$iid,
  nInst = max(polls$iid),
  date = polls$t,
  
  # Fundamental Model
  LA = length(unique(dat_sub$election)),
  L = length(unique(dat_sub$election)) + 1,
  N = length(election_res), #Number of observations
  Nobs = sum(complete.cases(c(election_res / 100))),
  Nmis = sum(!complete.cases(c(election_res / 100))),
  v_obs = c(election_res / 100)[ii_obs],  # Dependent variable
  v = c(election_res / 100),  # Dependent variable
  x = election_pred, # Predictors past elections
  K = ncol(election_pred),   # Number of predictors
  election = dat_sub$election,
  xE = as.matrix(election_pred_E), # Predictors for upcoming election
  b_prior = structural_inits$b, b0_prior = structural_inits$b0,
  ii_obs = ii_obs,
  ii_mis = ii_mis,
  s = as.vector(table(dat_sub$election))
  
)

cat("\n Estimating Model for Election", Election, "with a cutoff of ", cutoff, "\n") 
results <- stan(file = '../model_code/combined_model.stan', data = forstan, init = initlist,
            iter = nIter, chains = nChains, thin = 1, control = list(adapt_delta = 0.80, max_treedepth = 10) )

saveRDS(results, file = paste0("../../output/ger/draws/combined_model/res_brw_", Election,"_",cutoff,"_nohouse.RDS"))

res <- as.matrix(results)


draws_forecast_levels <- list() # Output Object

# Grep Levels, put in array and attach to list
levels <- array(NA,c(nIter / 2 * nChains, nParties, 366))

for(t in 1:366){
  sel_levels_temp <- paste0("alpha[", t, ",", 1:nParties, "]")
  levels[, , t] <- as.matrix(res[, sel_levels_temp])
}

draws_forecast_levels[["levels"]] <- levels


#Grep forcast and attach to list
sel_forecast <- paste0("forecast[", 1:nParties, "]")
draws_forecast_levels[["forecast"]] <- as.matrix(res[, sel_forecast])

# Attach partynames to list
draws_forecast_levels[["party_names"]] <- party_names

# Attach Polls used for estimation
draws_forecast_levels[["polls"]] <- polls

saveRDS(draws_forecast_levels, 
        file=paste0("../../output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,"_nohouse.RDS")
)
}
}



