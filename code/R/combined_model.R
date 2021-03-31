### ----------------------------------------------------------
### Election polling forecasting 
### implementation of backward random walk for multi-party set-ups
### lukas stoetzer
### 
### 1. Set-up and Estimate Model
###
### ----------------------------------------------------------
 
source("auxiliary/packages.r")  # Load required packages
source("auxiliary/functions.r") # Load additional functions





### 1. Set-up and estimate Model --------------------------------------------

      # Specifications
      Election <- 2017 # For 2017 election
      
      # Sampler
      nBurn <- 150000
      nThin <- 200
      nIter <- 200000
      nChains <- 5
      nSamples <- 2000/nChains
      thin <- 100
    
      
      # Sampler
      nBurn <- 150
      nThin <- 200
      nIter <- 200000
      nChains <- 5
      nSamples <- 20/nChains
      thin <- 100
      
    
    model_file="../Jags/combined_model.jags"
   
    save_var=c("alpha","S","house_effect","b0","b","vE","a", "S_shock", "forecast")
    
    # Load initial values for b0 and b from pre-trained structural model
    
    structural_inits <- readRDS("../../data/structural_inits.RDS")
    
    initlist <- replicate(nChains,structural_inits,simplify=FALSE)

  
    
    #### Data for Structural Model
    
    # Party ordering include 
    party_names <- c("cdu", "spd","lin","gru","fdp","afd","oth") # Back to OTH as reference 
    
    # Load Data
    load("../../data/ger/Structural/ger_model_df.RData")
    election_years <- unique(ger_df_long$year)
    election_years_id <- seq_along(election_years)
    election_years_df <- data.frame(year = election_years, election_id = election_years_id)
    
    # Predictors
    predictors = c("voteshare_l1", "chancellor_party", "polls_200_230")
    dependent = "voteshare"
    
    # Subset to complete Cases 
    which_cases <- complete.cases(ger_df_long[,c(predictors)])
    dat <- ger_df_long[,c("party",dependent, predictors, "election")]
    dat_sub <- dat[which_cases,]
    
    # Predictors for upcoming election
    election_pred_E <- dat[dat$election==19,c(predictors)]
    rownames(election_pred_E) <- dat[dat$election==19,"party"]
    election_pred_E <- election_pred_E[party_names,]
    election_pred_E[,c(1,3)] <- election_pred_E[,c(1,3)]/100
    # Election results in a vector
    election_res <- as.matrix(dat_sub[,dependent])
    
    # Predictor for past elections in matrix
    election_pred <- as.matrix(dat_sub[,predictors])
    election_pred[,c(1,3)] <- election_pred[,c(1,3)]/100
    # Index function gives first and last value  in vector of election results
    index <- t(sapply(unique(dat_sub$election),function(i) range(which(i==dat_sub$election))))
    
    
    #### Poll Data for Dynamic Model
    
      # Load Poll Data-base
        ger_polls <- read_dta("../../data/Polls/polls_btw.dta") %>% 
                      filter(election==Election) %>% # Filter Polls for Election
                      mutate(party = as.character(as_factor(party))) 
      
      # Cutoffs (until which Date polls should be included)
        cutoffs <- c(2, 8, 148, 116, 92, 64, 36)
        cutoffs <- c(2)
        
        for(cutoff in cutoffs){
        #cutoff <- 146
        
      
        
      # Set time_window for data 
        max_days_to_election <- 365 # Start point up from which Polls are included
        time_window <- max_days_to_election:cutoff  # time window
      
      
      
      # Prepare Data
        polls <- ger_polls %>%
            filter(days_to_election %in% time_window) %>% # Filter Polls in time-window
            select(party, support, days_to_election, sample_size, institute) %>% # Select important information
            mutate(t= time_window[1] - days_to_election +1 ) %>% # create t variable from 1:366
            arrange(desc(days_to_election)) %>% # Arrange according to days to election
            mutate(iid = match(institute , sort(unique(institute)))) %>% # Creat Institute id
            spread(party, support)  %>% # Reshape to wide format
            na.omit() # Omit missing
    
      # Prepare Data for Jags
        Y <- round(as.matrix(polls[,party_names]/100) * 
                     polls$sample_size) # Grep existing Parties and transform share to number
        NObs <- apply(Y,1,sum) # Number of obserations
        nParties <-ncol(Y) # Number of parties
  
  
        

   ### Combine Data for Structural Model and Dynamic Moel
        
      # Jags List  
        forJags <- list(
          
                        # Dynamic Model
                          y = Y,
                          nParties = nParties,
                          nPeriods =  time_window[1]+1, 
                          nPolls = nrow(Y),
                          iid = polls$iid,
                          nInst = max(polls$iid),
                          date = polls$t,
                          size = NObs, 
                          R0 = diag(rep(1,(nParties-1)))*0.01, n0 = 7, # Prior on Wishardt
                          shock = solve(diag(rep(1,(nParties-1)))), n_shock = 30.8642,
                          
                        # Fundamental Model
                          LA = nrow(index),
                          L = nrow(index)+1,# Number of elections
                          N = length(election_res), #Number of observations
                          v = c(election_res/100),  # Dependent variable
                          ind = index,        # Create index function (which observation part of election row l)
                          x = election_pred, # Predictors past elections
                          K = ncol(election_pred),   # Number of predictors
                          election = dat_sub$election,
                          xE = as.matrix(election_pred_E), # Predictors for upcoming election
                          b_prior = structural_inits$b, b0_prior = structural_inits$b0
        
                        )
  

    
    ### Run MCMC Sampler
        
        
        # # Set-up Jags Model
        # jags.mod2 <- jags.model(file = model_file,
        #                         data=forJags,
        #                         n.chains=2,
        #                         n.adapt=1000)
        # 
        # # Update using Burn-in Iterations
        # update(jags.mod2,nBurn)
        # 
        # # Sample from Model
        # res_brw2 <- coda.samples(jags.mod2,
        #                          n.iter=nSamples*thin,thin=thin,
        #                          variable.names=save_var)
        
        # Print Model estimation
        cat("\n Estimating Model for Election", Election, "with a cutoff of ", cutoff, " and new fix \n") 
      # Ls: I get an error 
      # MN: TODO inits
      cl <- makeCluster(ifelse(nChains<(detectCores()-1),nChains, (detectCores()-1)))
      # Run the chains in parallel rjags models (4 models
      # with 2 chains each) on this cluster:
      results <- run.jags(model = model_file, n.chains = nChains, data = forJags, inits = initlist,
                          method = "rjparallel", cl = cl, monitor = save_var, sample = nSamples, thin = thin, burnin = nBurn, adapt = 30000)
      stopCluster(cl)

      
      saveRDS(results, file = paste0("../../output/draws/res_brw_", Election,"_",cutoff,".RDS"))
      
        
      # Diagnostics 
      
      # sink(paste("log/draws-",cutoff,".text",sep=""))
      # 
      # s <- paste("alpha[",1:(365-158),",",1:7,"]",sep="")
      # gelman.diag(results$mcmc[,s]) #
      # 
      # s <- paste("alpha[",(365-160):366,",",1:7,"]",sep="")
      # gelman.diag(results$mcmc[,s]) # 
      # 
      # s <- paste("alpha[",366,",",7,"]",sep="")
      # gelman.diag(results$mcmc[,s]) # 
      # 
      # s <- paste("s[",1:6,"]",sep="")
      # gelman.diag(results$mcmc[,s]) # 
      # s <- paste("house_effect[",1,",",1:6,"]",sep="")
      # gelman.diag(results$mcmc[,s]) # 
      # 
      # s <- paste("forcast[",1:6,"]",sep="")
      # gelman.diag(results$mcmc[,s])
      # summary(results$mcmc[,s])
      # 
      # s <- paste("forcast[",7,"]",sep="")
      # gelman.diag(results$mcmc[,s])
      # summary(results$mcmc[,s])
      # sink()
      
      draws_forecast_levels <- list() # Output Object
      
      # Grep Levels, put in array and attach to list
      levels <- array(NA,c(nSamples*nChains,nParties,366))
      
      for(t in 1:366){
        sel_levels_temp <- paste("alpha[",t,",",1:nParties,"]",sep="")
        levels[,,t] <- as.matrix(results$mcmc[,sel_levels_temp])
      }
      
      draws_forecast_levels[["levels"]] <- levels
      
      
      #Grep forcast and attach to list
      sel_forecast <- paste("forecast[",1:nParties,"]",sep="")
      draws_forecast_levels[["forecast"]] <- as.matrix(results$mcmc[,sel_forecast])

      # Attach partynames to list
      draws_forecast_levels[["party_names"]] <- party_names
      
      # Attach Polls used for estimation
      draws_forecast_levels[["polls"]] <- polls
      
      saveRDS(draws_forecast_levels, 
              file=paste0("../../output/draws/draws_forcast_levels_",Election,"_",cutoff,".RDS")
      )
      
}  
      
      
     
        
    