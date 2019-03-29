############################
### Garcia et al. (2005) ###
# MSE Development Boosting #
############################

rm(list = ls())

start.time <- Sys.time()

library(mboost)

set.seed(123)

### Container & Simulation Setup ------
n_sim <- 50
n_boost <- 300
mse <- matrix(NA, nrow = n_sim, ncol = n_boost)

train_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_train.rds")
test_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_test.rds")

### mc simulation
for (mc_iter in seq(n_sim)){
  # mc_iter <- 1
  
  gam1 <- gamboost(DEXfat~., data = train_splits[[mc_iter]], control = boost_control(mstop = n_boost, nu = 0.1))
  
  for (boost_iter in seq(n_boost)){
    gamboost_prediction <- predict(gam1[boost_iter], newdata = test_splits[[mc_iter]])
    mse[mc_iter, boost_iter] <- mean((gamboost_prediction-test_splits[[mc_iter]]$DEXfat)^2)
  }
}

# test plot
plot(seq(n_boost), apply(mse, 2, mean), type = "l", ylim = c(5, 18))
saveRDS(mse, file = "../99_Stored_Data/garcia_mse_development_gam.rds")
