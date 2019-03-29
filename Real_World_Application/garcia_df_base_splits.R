############################
### Garcia et al. (2005) ###
### Degrees of freedom #####
############################

rm(list = ls())

library(mboost)

set.seed(123)

train_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_train.rds")
test_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_test.rds")


### Container & Simulation Setup ------
n_sim <- 50
n_df <- 8
mse <- matrix(NA, nrow = n_sim, ncol = n_df)

col_df_iter <- 1
for (df_iter in c(2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)){
  
  for (mc_iter in seq(n_sim)){
    # mc_iter <- 1
  
    gam1 <- gamboost(DEXfat~., 
                     data = train_splits[[mc_iter]], 
                     dfbase = df_iter,
                     control = boost_control(mstop = 500, nu = 0.1))
    m_stop_aic <- mstop(aic <- AIC(gam1))
    gamboost_prediction <- predict(gam1[m_stop_aic], newdata = test_splits[[mc_iter]])
    
    mse[mc_iter, col_df_iter] <- mean((gamboost_prediction-test_splits[[mc_iter]]$DEXfat)^2)
  }
  
  cat(paste0("Done with iteration: ", col_df_iter, "\n"))
  col_df_iter <- col_df_iter + 1
}

saveRDS(mse, file = "../99_Stored_Data/garcia_degrees_of_freedom.rds")
boxplot(mse, use.cols = TRUE)