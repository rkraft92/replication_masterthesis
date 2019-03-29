############################
### Garcia et al. (2005) ###
############################

rm(list = ls())

start.time <- Sys.time()

library(mboost)

set.seed(123)

### Container & Simulation Setup ------
n_sim <- 50

train_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_train.rds")
test_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_test.rds")

mc_iter <- 10

### GAMBoost model construction & prediction ------
gam1 <- gamboost(DEXfat~., data = train_splits[[mc_iter]], control = boost_control(mstop = 500, nu = 0.1))
m_stop_cvm <- mstop(aic <- AIC(gam1))

# export figure
pdf("../../06_Plots/Real_World_Application/garcia_partial_effects_gam.pdf")
par(mfrow = c(3,2))
plot(gam1[m_stop_cvm], ylim = c(-6,6))
dev.off()