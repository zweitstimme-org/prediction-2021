source("packages.r")  # Load required packages
source("functions.r") # Load additional functions

# Party ordering include 
party_names <- c("cdu", "spd","lin","gru","fdp","afd","oth") # Back to OTH as reference // TRY CDU AS REFERENCE

res <- as.mcmc.list(readRDS("tmp/res_brw.RDS"))

# 
plot(res[,grep("alpha\\[366,",colnames(as.matrix(res)))])
plot(res[,grep("vE",colnames(as.matrix(res)))])
plot(res[,grep("b",colnames(as.matrix(res)))])


# Plot b
df <- t(apply(as.matrix(res[,grep("b",colnames(as.matrix(res)))]),2,quantile,c(0.95,0.5,0.05)))
df <- dcast(melt(df), Var1 ~ Var2) 
df$t <- 1:19; df$par <- as.factor(sort(rep(0:3,19))); levels(df$par) <- c("cons","vote","chancellor","polls")

names(df) <- c("des","high","mid","low","t","par")

ggplot(df) + geom_line(aes(y=mid,x=t)) + geom_ribbon(aes(ymin=low,ymax=high,x=t),alpha=0.2) + 
  facet_grid(par~., scales="free") + geom_hline(yintercept = 0) + theme_minimal()

# Plot states
nParties <- 7
# Array to save latent states alpha
levels<- array(NA,c(2000,nParties,366))

for(t in 1:366){
  sel_levels_temp <- paste("alpha[",t,",",1:nParties,"]",sep="")
  levels[,,t] <- as.matrix(res[,sel_levels_temp])
}

# Create Data-frame
l <- apply(levels,2:3,quantile,c(0.975,0.5,0.025))
ld <- melt(l, varnames = c("ci","party","t"))
levels(ld$ci) <- c("high","mid","low")
df_plot <- dcast(ld, t + party ~ ci)
df_plot$party <- as.factor(df_plot$party); levels(df_plot$party) <- party_names

## Get Poll Data
load("../Data/Polls/polls_comb_results.RData")

# See Levels
polls_long <- ger_polls_results %>%
  mutate(t=    365  - days_to_election +1 ) %>% 
  filter(days_to_election %in%  365:1) %>%
  filter(year==2017) %>%
  filter(!is.na(support)) %>%
  select(party, support, days_to_election, sample_size, institute, t) 

polls_long$institute <- factor(polls_long$institute)
levels(polls_long$institute) <- c("IfD Allensbach", "Emnid", "F'Gruppe Wahlen", 
                                  "forsa", "GMS", "Infratest dimap", "INSA")

# Change order
df_plot <- transform(df_plot, party=factor(party,levels=c("cdu","spd","lin","gru","fdp","afd","oth")))
polls_long <- transform(polls_long, party=factor(party,levels=c("cdu","spd","lin","gru","fdp","afd","oth")))

ggplot() + 
  geom_line(data=filter(df_plot, party != "oth"),aes(y = mid*100, x = t, group=party)) + 
  geom_ribbon(data=filter(df_plot, party != "oth"),aes(y = mid*100,ymin=low*100, ymax=high*100, x = t, fill=party),alpha=0.2) +
  # geom_ribbon(data=df_plot,aes(y = mid*100,ymin=low3*100, ymax=high3*100, x = t),alpha=0.3) +
  geom_point(data=polls_long,aes(x=t,y=support, pch=institute),alpha=0.2)+
  xlab("Tage bis zur Wahl") + ylab("Stimmenanteil (%)") + facet_wrap(~ party, scales="free") + 
  scale_x_continuous(
    breaks = max(df_plot$t) - c(365,200,146,1),
    label = c(365,200,146,1)
  ) +
  theme_bw()+ theme(legend.title=element_blank())  +
  scale_shape_manual(values=seq(0,15))




