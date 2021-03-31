# import data

df_nz <- readRDS("data/nz/Structural/nz_structural.RDS")

## plot relationship between response and predictors --------------

par(oma=c(0,0.5,0.5,0.5))
par(mar=c(4, 4, 3, 0))
par(pty="s")
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))

# Previous Voteshare
plot(df_nz$voteshare_l1, df_nz$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "(a)", xlim = c(0, 50), ylim = c(0, 50), bty = "n")
axis(1, seq(0, 100, 10), seq(0, 100, 10))
axis(1, 25, "Vote Share (%), Previous Election", line = 1, tick = F)
axis(2, seq(0, 100, 10), seq(0, 100, 10), las = 1)
axis(2, 25, "Vote Share (%)", line = 1, tick = F)
# run model, add regression line
model_out <- lm(voteshare ~ voteshare_l1 - 1, data = df_nz)
model_out_aug <- augment(model_out)

abline(model_out, lty = 2)
# identify important outliers
obs_id <- abs(model_out_aug$.std.resid) > 1.53
df_nz[obs_id,]

points(model_out_aug$voteshare_l1[obs_id], model_out_aug$voteshare[obs_id], pch = 20)

# plot labels of outliers based on resid or cooksd 
label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
eng_label <- c("Labour '99", "National '02", "National '05", "Others '05")
text(model_out_aug$voteshare_l1[obs_id], model_out_aug$voteshare[obs_id], label = eng_label, cex = .7, pos = label_position[obs_id], offset = .47)
grid()

# Polls
plot(df_nz$polls_200_230, df_nz$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "(b)", xlim = c(0, 60), ylim = c(0, 60), bty = "n")
axis(1, seq(0, 100, 10), seq(0, 100, 10))
axis(1, 30, "Vote Intention in Polls (%), \n200-230 days prior to election", line = 2, tick = F)
axis(2, seq(0, 100, 10), seq(0, 100, 10), las = 1)
axis(2, 30, "Vote Share (%)", line = 1, tick = F)
# run model, add regression line
model_out <- lm(voteshare ~ polls_200_230 - 1, data = df_nz)
model_out_aug <- augment(model_out)

abline(model_out, lty = 2)
# identify important outliers
obs_id <- abs(model_out_aug$.std.resid) > 1.53
points(model_out_aug$polls_200_230[obs_id], model_out_aug$voteshare[obs_id], pch = 20)
# plot labels of outliers based on resid or cooksd 
label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
eng_label <- c("Others '08", "Labour '14")
text(model_out_aug$polls_200_230[obs_id], model_out_aug$voteshare[obs_id], label = eng_label, cex = .7, pos = label_position[obs_id], offset = .47)
grid()

par(mar=c(3, 15, 3, 11))

# Prime Minister's Party
dat <- filter(df_nz, party == "lab" | party == "nat")
dat$chancellor_party_lab <- ifelse(dat$chancellor_party == 0, "keine Kanzlerpartei", "Kanzlerpartei")
plot(dat$chancellor_party,  dat$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "(c)", xlim = c(-.5, 1.5), ylim = c(20, 52), bty = "n")
axis(2, seq(0, 100, 10), seq(0, 100, 10), las = 1)
axis(1, seq(0, 1, 1), c("not \n Prime Minister's Party", "Prime Minister's \n Party"), cex.axis = 0.7, tick = F)
axis(2, 35, "Vote Share (%)", line = 1, tick = F)

# run model, add regression line
model_out <- lm(voteshare ~ chancellor_party, data = dat)
model_out_aug <- augment(model_out)

abline(model_out, lty = 2)
# identify important outliers

obs_id <- abs(model_out_aug$.std.resid) > 1.3
points(model_out_aug$chancellor_party[obs_id], model_out_aug$voteshare[obs_id], pch = 20)
# plot labels of outliers based on resid or cooksd 
eng_label <- c("National '02", "National '08")
label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
text(model_out_aug$chancellor_party[obs_id], model_out_aug$voteshare[obs_id], label = eng_label, cex = .9, pos = label_position, offset = .47)
grid()