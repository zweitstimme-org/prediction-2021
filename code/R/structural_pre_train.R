### ----------------------------------------------------------
### Election polling forecasting 
### implementation of backward random walk for multi-party set-ups
### lukas stoetzer
### 
### 1. Set-up and Estimate Model
###
### ----------------------------------------------------------

source("../packages.r")  # Load required packages
source("../functions.r") # Load additional functions

### 0. Pre-train Structual Model

# Specifications
Election <- 2017 # For 2017 election

# Sampler
nBurn <- 350000
nThin <- 200
nIter <- 200000
nChains <- 5
nSamples <- 2000/nChains
thin <- 100


model_file="structural_pre_train_priors.jags"
save_var=c("b0","b","vE","a")



### Data for Structural Model

# Load Data
load("../../Data/Structural/ger_model_df.RData")
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

party_names <- c("cdu", "spd","lin","gru","fdp","afd","oth") # Back to OTH as reference 
election_pred_E <- election_pred_E[party_names,]
election_pred_E[,c(1,3)] <- election_pred_E[,c(1,3)]/100
# Election results in a vector
election_res <- as.matrix(dat_sub[,dependent])

# Predictor for past elections in matrix
election_pred <- as.matrix(dat_sub[,predictors])
election_pred[,c(1,3)] <- election_pred[,c(1,3)]/100
# Index function gives first and last value  in vector of election results
index <- t(sapply(unique(dat_sub$election),function(i) range(which(i==dat_sub$election))))

nParties <- length(party_names) # Number of parties
# Jags List  
forJags <- list(
  
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
  nParties = nParties
)


# Ls: I get an error 
# MN: TODO inits
cl <- makeCluster(ifelse(nChains<(detectCores()-1),nChains, (detectCores()-1)))
# Run the chains in parallel rjags models (4 models
# with 2 chains each) on this cluster:
results <- run.jags(model = model_file, n.chains = nChains, data = forJags,
                    method = "rjparallel", cl = cl, monitor = save_var, sample = nSamples, thin = thin, burnin = nBurn, adapt = 30000)
stopCluster(cl)

saveRDS(results, file = paste0("results/structural_pre_train_", Election,"_weaklyInformed.RDS"))

res <- results$mcmc

mcmcplot(res)

jags_matrix <- as.matrix(res)

1/apply(jags_matrix,2, var)[grepl("b0\\[",names(apply(jags_matrix,2, var)))]
1/apply(jags_matrix,2, var)[grepl("b\\[",names(apply(jags_matrix,2, var)))]


apply(jags_matrix,2, mean)[grepl("vE\\[",names(apply(jags_matrix,2, mean)))]
apply(jags_matrix,2, quantile, c(1/12,11/12))[,grepl("vE\\[",names(apply(jags_matrix,2, mean)))]
jags_summary_df <- jags_summary(jags_matrix)


b_mean <- filter(jags_summary_df, str_detect(var, "b\\[")) %>% magrittr::extract2("mean")

b_mean <- matrix(b_mean, ncol = 3)


b_0_mean <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("mean")


structural_inits <- list(b = b_mean, b0 = b_0_mean)

saveRDS(structural_inits, "structural_inits.RDS")

b_0_95lo <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("q95lo")
b_0_95hi <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("q95hi")

b_voteshare_l1_mean <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},1\\]")) %>% magrittr::extract2("mean")
b_voteshare_l1_95lo <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},1\\]")) %>% magrittr::extract2("q95lo")
b_voteshare_l1_95hi <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},1\\]")) %>% magrittr::extract2("q95hi")

b_chancellor_party_mean <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},2\\]")) %>% magrittr::extract2("mean")
b_chancellor_party_95lo <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},2\\]")) %>% magrittr::extract2("q95lo")
b_chancellor_party_95hi <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},2\\]")) %>% magrittr::extract2("q95hi")

b_polls_200_230_mean <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},3\\]")) %>% magrittr::extract2("mean")
b_polls_200_230_95lo <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},3\\]")) %>% magrittr::extract2("q95lo")
b_polls_200_230_95hi <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},3\\]")) %>% magrittr::extract2("q95hi")

dev.off()
pdf(file="output/structural_model_coefplot_pre_train.pdf", height=8, width=6, family="URWTimes")
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
plot(election_years_id[-1], b_voteshare_l1_mean[-1], type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(floor(min_yaxis), ceiling(max_yaxis)), main = "Vote Share Previous Election", bty = "n")
axis(1, election_years_id[-1], labels = FALSE)
text(x = election_years_id[-1], y = floor(min_yaxis)-((ceiling(max_yaxis)-floor(min_yaxis))/8), labels = election_years[-1], par("usr")[1]-.5, srt = 45, xpd = TRUE)
axis(2, seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = FALSE)
text(y = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), par("usr")[1]-.25, srt = 0, pos = 2, xpd = TRUE)
polygon(x = c(election_years_id[-1], rev(election_years_id[-1])), 
        y = c(b_voteshare_l1_95hi[-1], rev(b_voteshare_l1_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-1], b_voteshare_l1_mean[-1])
grid()
abline(0,0)
# b_polls_200_230
min_yaxis <- round(min(b_polls_200_230_95lo[-1]), 1) - .1
max_yaxis <- round(max(b_polls_200_230_95hi[-1]), 1) + .1
plot(election_years_id[-1], b_polls_200_230_mean[-1], type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(floor(min_yaxis), ceiling(max_yaxis)), main = "Vote Intention in Polls, 200-230 days prior to election", bty = "n")
axis(1, election_years_id[-1], labels = FALSE)
text(x = election_years_id[-1], y = floor(min_yaxis)-((ceiling(max_yaxis)-floor(min_yaxis))/8), labels = election_years[-1], par("usr")[1]-.5, srt = 45, xpd = TRUE)
axis(2, seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = FALSE)
text(y = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), par("usr")[1]-.25, srt = 0, pos = 2, xpd = TRUE)

polygon(x = c(election_years_id[-1], rev(election_years_id[-1])), 
        y = c(b_polls_200_230_95hi[-1], rev(b_polls_200_230_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-1], b_polls_200_230_mean[-1])
grid()
abline(0,0)
# b_chancellor_party
min_yaxis <- round(min(b_chancellor_party_95lo[-1]), 0) - .1
max_yaxis <- round(max(b_chancellor_party_95hi[-1]), 0) + .1
plot(election_years_id[-1], b_chancellor_party_mean[-1], type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(floor(min_yaxis), ceiling(max_yaxis)), main = "Chancellor's Party", bty = "n")
axis(1, election_years_id[-1], labels = FALSE)
text(x = election_years_id[-1], y = floor(min_yaxis)-((ceiling(max_yaxis)-floor(min_yaxis))/8), labels = election_years[-1], par("usr")[1]-.5, srt = 45, xpd = TRUE)
axis(2, seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = FALSE)
text(y = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), labels = seq(floor(min_yaxis), ceiling(max_yaxis), length.out = 5), par("usr")[1]-.25, srt = 0, pos = 2, xpd = TRUE)

polygon(x = c(election_years_id[-1], rev(election_years_id[-1])), 
        y = c(b_chancellor_party_95hi[-1], rev(b_chancellor_party_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-1], b_chancellor_party_mean[-1])
grid()
abline(0,0)
mtext("Regression Coefficient", 2, 1, outer=TRUE, las=0)
mtext("Structural Pre-Train", side = 3, 1, outer=TRUE, las=0, cex = 1.2, font = 2)
dev.off()



