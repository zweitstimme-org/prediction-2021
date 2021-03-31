# Estimated Correlation Matrix for Evolution Variance

Election <- 2017
cutoff <- 2
tmp <- readRDS(file=paste0("output/ger/draws/combined_model/res_brw_",Election,"_",cutoff,".RDS"))

tmp1 <- readRDS(file=paste0("output/ger/draws/combined_model/draws_forcast_levels_",Election,"_",cutoff,".RDS"))

tmp1 <- tmp1$party_names
tmp <- as.matrix(tmp)


S_m <- round(apply(tmp[,grep("^S_cor", colnames(tmp))],2, mean) %>% matrix(ncol = 6, nrow = 6) ,2)
S_95 <- paste0("(",round(apply(tmp[,grep("^S_cor", colnames(tmp))],2, quantile, 0.025) %>% matrix(ncol = 6, nrow = 6) ,2), 
               "; ",
               round(apply(tmp[,grep("^S_cor", colnames(tmp))],2, quantile, 0.975) %>% matrix(ncol = 6, nrow = 6) ,2), ")") %>%  matrix(ncol = 6, nrow = 6)

S_table <- NULL
for(i in 1:6){
  S_table <- rbind(S_table, S_m[i,], S_95[i,])
}

colnames(S_table) <- tmp1[1:6]

rownames(S_table) <- rep(c("mean", "95 % CI"),6)

print(xtable(S_table), file = "output/paper/appendix_table4.tex")