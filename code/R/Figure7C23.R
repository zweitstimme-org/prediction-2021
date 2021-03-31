nz_df_long <- readRDS("data/nz/Structural/nz_structural.RDS")

election_years <- unique(nz_df_long$year)
election_years_id <- seq_along(election_years)

results <- readRDS("output/nz/draws/structural_pre_train/2017_structural_pre_train_stan.RDS")

res <- as.matrix(results)


jags_matrix <- as.matrix(res)

jags_summary_df <- jags_summary(jags_matrix)


# Make coefficient plot for upcoming election

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
  
  
  par(mar=c(4,4,2,0)+.1)
  par(oma=c(0,4,4,0)+.1)
  par(mfrow=c(3,1))

  
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


