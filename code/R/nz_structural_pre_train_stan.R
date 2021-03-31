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


model_file <- "../model_code/nz_structural_pre_train.stan"

Elections <- c(2011, 2014, 2017)

for(Election in Elections){


### Data for Structural Model

# Load Data
nz_df_long <- readRDS("../../data/nz/Structural/nz_structural.RDS")

election_years <- unique(nz_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

election_id <- election_years_df$election_id[election_years_df$year == Election]

# Predictors
predictors <- c("voteshare_l1", "chancellor_party", "polls_200_230")
dependent<- "voteshare"

# Subset to complete Cases 
which_cases <- complete.cases(nz_df_long[, c(predictors)])
dat <- nz_df_long[, c("party",dependent, predictors, "election")]
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
  election = dat_sub$election-2,
  nParties = nParties,
  ii_obs = ii_obs,
  ii_mis = ii_mis,
  s = nParties_vec
)


results <- stan(file = model_file, data = forstan,
            iter = nIter, chains = nChains, thin = 1, control = list(adapt_delta = 0.99, max_treedepth = 15))


saveRDS(results, file = paste0("../../output/nz/draws/structural_pre_train/", Election, "_structural_pre_train_stan.RDS"))

res <- as.matrix(results)


jags_matrix <- as.matrix(res)

# What is this?
# 1/apply(jags_matrix,2, var)[grepl("b0\\[",names(apply(jags_matrix,2, var)))]
# 1/apply(jags_matrix,2, var)[grepl("b\\[",names(apply(jags_matrix,2, var)))]


structural_forecast <- jags_matrix[,grepl("y_mis\\[",colnames(jags_matrix))]
colnames(structural_forecast) <- dat_sub$party[!complete.cases(dat_sub$voteshare)]

print(apply(structural_forecast,2,mean))

saveRDS(structural_forecast, file = paste0("../../output/nz/forecasts/structural_pre_train/", Election, "_structural_forecast.RDS"))

apply(jags_matrix,2, mean)[grepl("y_mis\\[",names(apply(jags_matrix,2, mean)))]
apply(jags_matrix,2, quantile, c(1/12,11/12))[,grepl("y_mis\\[",names(apply(jags_matrix,2, mean)))]
jags_summary_df <- jags_summary(jags_matrix)


b_mean <- filter(jags_summary_df, str_detect(var, "^b\\[")) %>% magrittr::extract2("mean")

b_mean <- matrix(b_mean, ncol = 3)


b_0_mean <- filter(jags_summary_df, str_detect(var, "^b0\\[")) %>% magrittr::extract2("mean")


structural_inits <- list(b = b_mean, b0 = b_0_mean)

saveRDS(structural_inits, paste0("../../data/nz/structural_inits/", Election,"_structural_inits.RDS"))

}
# Make coefficient plot for upcoming election

if(Election == upcoming_election){

b_0_95lo <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("q95lo")
b_0_95hi <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("q95hi")

b_voteshare_l1_mean <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},1\\]")) %>% magrittr::extract2("mean")
b_voteshare_l1_95lo <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},1\\]")) %>% magrittr::extract2("q95lo")
b_voteshare_l1_95hi <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},1\\]")) %>% magrittr::extract2("q95hi")

b_chancellor_party_mean <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},2\\]")) %>% magrittr::extract2("mean")
b_chancellor_party_95lo <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},2\\]")) %>% magrittr::extract2("q95lo")
b_chancellor_party_95hi <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},2\\]")) %>% magrittr::extract2("q95hi")

b_polls_200_230_mean <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},3\\]")) %>% magrittr::extract2("mean")
b_polls_200_230_95lo <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},3\\]")) %>% magrittr::extract2("q95lo")
b_polls_200_230_95hi <- filter(jags_summary_df, str_detect(var, "^b\\[.{1,2},3\\]")) %>% magrittr::extract2("q95hi")


pdf(file=paste0("../../output/nz/plots/structural_pre_train/", Election,"_coefplot.pdf"), height=8, width=6, family="URWTimes")
par(mar=c(4,4,2,0)+.1)
par(oma=c(0,4,4,0)+.1)
par(mfrow=c(3,1))
#par(xaxs='i')

# # b0
# min_yaxis <- round(min(b_0_95lo), 1) - .1
# max_yaxis <- round(max(b_0_95hi), 1) + .1
# plot(election_years_id, b_0_mean, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(min_yaxis, max_yaxis), main = "Konstante")
# axis(1, election_years_id, labels = FALSE)
# text(x = election_years_id, y = -.43, labels = election_years, par("usr")[1]+.5, srt = 45, xpd = TRUE)
# axis(2, seq(min_yaxis, max_yaxis, 2), labels = FALSE)
# text(y = seq(min_yaxis, max_yaxis, 2), labels = seq(min_yaxis, max_yaxis, 2), par("usr")[1]-.25, srt = 0, pos = 2, xpd = TRUE)
# polygon(x = c(election_years_id, rev(election_years_id)), 
#         y = c(b_0_95hi, rev(b_0_95lo)),
#         col = rgb(.8, .8, .8, .5),
#         border = NA)
# lines(election_years_id, b_0_mean)
# grid()

# b_voteshare_l1
min_yaxis <- round(min(b_voteshare_l1_95lo[-1]), 1) - .1
max_yaxis <- round(max(b_voteshare_l1_95hi[-1]), 1) + .1
plot(election_years_id[-(1:3)], b_voteshare_l1_mean[-1], type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(floor(min_yaxis), ceiling(max_yaxis)), main = "Vote Share Previous Election", bty = "n")
axis(1, election_years_id[-(1:3)], labels = FALSE)
text(x = election_years_id[-(1:3)], y = floor(min_yaxis)-((ceiling(max_yaxis)-floor(min_yaxis))/8), labels = election_years[-(1:3)], par("usr")[1]-2.5, srt = 45, xpd = TRUE)
axis(2, seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = FALSE)
text(y = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), par("usr")[1]-0.05, srt = 0, pos = 2, xpd = TRUE)
polygon(x = c(election_years_id[-(1:3)], rev(election_years_id[-(1:3)])), 
        y = c(b_voteshare_l1_95hi[-1], rev(b_voteshare_l1_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-(1:3)], b_voteshare_l1_mean[-1])
grid()
abline(0,0)
# b_polls_200_230
min_yaxis <- round(min(b_polls_200_230_95lo[-1]), 1) - .1
max_yaxis <- round(max(b_polls_200_230_95hi[-1]), 1) + .1
plot(election_years_id[-(1:3)], b_polls_200_230_mean[-1], type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(floor(min_yaxis), ceiling(max_yaxis)), main = "Vote Intention in Polls, 200-230 days prior to election", bty = "n")
axis(1, election_years_id[-(1:3)], labels = FALSE)
text(x = election_years_id[-(1:3)], y = floor(min_yaxis)-((ceiling(max_yaxis)-floor(min_yaxis))/8), labels = election_years[-(1:3)], par("usr")[1]-2.5, srt = 45, xpd = TRUE)
axis(2, seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = FALSE)
text(y = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), par("usr")[1]-.05, srt = 0, pos = 2, xpd = TRUE)

polygon(x = c(election_years_id[-(1:3)], rev(election_years_id[-(1:3)])), 
        y = c(b_polls_200_230_95hi[-1], rev(b_polls_200_230_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-(1:3)], b_polls_200_230_mean[-1])
grid()
abline(0,0)
# b_chancellor_party
min_yaxis <- round(min(b_chancellor_party_95lo[-1]), 0) - .1
max_yaxis <- round(max(b_chancellor_party_95hi[-1]), 0) + .1
plot(election_years_id[-(1:3)], b_chancellor_party_mean[-1], type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(floor(min_yaxis), ceiling(max_yaxis)), main = "Prime Minister's Party", bty = "n")
axis(1, election_years_id[-(1:3)], labels = FALSE)
text(x = election_years_id[-(1:3)], y = floor(min_yaxis)-((ceiling(max_yaxis)-floor(min_yaxis))/8), labels = election_years[-(1:3)], par("usr")[1]-2.5, srt = 45, xpd = TRUE)
axis(2, seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = FALSE)
text(y = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), par("usr")[1]-.05, srt = 0, pos = 2, xpd = TRUE)

polygon(x = c(election_years_id[-(1:3)], rev(election_years_id[-(1:3)])), 
        y = c(b_chancellor_party_95hi[-1], rev(b_chancellor_party_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-(1:3)], b_chancellor_party_mean[-1])
grid()
abline(0,0)
mtext("Regression Coefficient", 2, 1, outer=TRUE, las=0)
#mtext("Structural Pre-Train", side = 3, 1, outer=TRUE, las=0, cex = 1.2, font = 2)
dev.off()
}


