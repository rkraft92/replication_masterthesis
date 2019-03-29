############################
### Garcia et al. (2005) ###
############################

rm(list = ls())

start.time <- Sys.time()

library(dplyr)
library(mboost)
library(earth)
library(gam)
library(MASS)
library(glmnet)
library(caret)

set.seed(123)

### Container & Simulation Setup ------
n_sim <- 50
n_algos <- 8
mse <- matrix(NA, nrow = n_sim, ncol = n_algos)

train_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_train.rds")
test_splits <- readRDS(file = "../99_Stored_Data/garcia_data_splits_test.rds")


for (mc_iter in seq(n_sim)){
  # mc_iter <- 1
  
  ### Fitting model from paper & prediction -------
  lm1 <- lm(DEXfat ~ hipcirc + kneebreadth + anthro3a, data = train_splits[[mc_iter]])
  lm_prediction <- predict(lm1, newdata = test_splits[[mc_iter]])
  
  #Fwd Var Selection
  fit_start <- lm(DEXfat~1, data = train_splits[[mc_iter]])
  fit_end <- lm(DEXfat~., data = train_splits[[mc_iter]])
  step_fit <- stepAIC(fit_start, direction="forward", scope=list(lower=fit_start, upper=fit_end), trace = F)
  step_pred <- predict(step_fit, test_splits[[mc_iter]])
  
  ### LASSO -------
  lasso_fit <- cv.glmnet(x=as.matrix(train_splits[[mc_iter]][,c(2:10)]), y = train_splits[[mc_iter]][,c(1)], alpha = 1, nfolds = 10)
  lasso_pred <-predict(lasso_fit, newx = as.matrix(test_splits[[mc_iter]][,c(2:10)]), s = "lambda.min")
  
  ### MARS -------
  mars_fit <- earth(DEXfat ~ .,  data = train_splits[[mc_iter]])  
  mars_pred <- predict(mars_fit, test_splits[[mc_iter]])
  
  #### GAM fit with inclusion strategy ------
  gam_fit <- gam(DEXfat ~ 1, family = "gaussian", data = train_splits[[mc_iter]])
  gam_scope <- gam.scope(frame = train_splits[[mc_iter]], response = 1, arg = "df=5")
  gam_step <- step.gam(gam_fit, gam_scope, direction = "forward", trace = FALSE)
  gam_pred <- predict(gam_step, test_splits[[mc_iter]])
  
  #### Stump fit (btree = stump per default) -----
  stump_fit <- gamboost(DEXfat ~ ., 
                        data = train_splits[[mc_iter]], 
                        baselearner = "btree",
                        control = boost_control(mstop = 500, nu = 0.1))
  m_stop_stump <- mstop(cvr <- cvrisk(stump_fit))
  stump_pred <- predict(stump_fit[m_stop_stump], test_splits[[mc_iter]])
  
  ### GAMBoost model construction & prediction ------
  gam1 <- gamboost(DEXfat~., data = train_splits[[mc_iter]], control = boost_control(mstop = 500, nu = 0.1))
  m_stop_cvm <- mstop(aic <- AIC(gam1))
  gamboost_prediction <- predict(gam1[m_stop_cvm], newdata = test_splits[[mc_iter]])
  
  ### GLMBoost model construction & prediction ------
  glm1 <- glmboost(DEXfat~., data = train_splits[[mc_iter]], control = boost_control(mstop = 500, nu = 0.1), center = TRUE)
  m_stop_aic <- mstop(aic <- AIC(glm1))
  glmboost_prediction <- predict(glm1[m_stop_aic], newdata = test_splits[[mc_iter]])
  
  ### MSE comparison
  mse[mc_iter, 1] <- mean((lm_prediction-test_splits[[mc_iter]]$DEXfat)^2)
  mse[mc_iter, 2] <- mean((gamboost_prediction-test_splits[[mc_iter]]$DEXfat)^2)
  mse[mc_iter, 3] <- mean((glmboost_prediction-test_splits[[mc_iter]]$DEXfat)^2)
  mse[mc_iter, 4] <- mean((step_pred-test_splits[[mc_iter]]$DEXfat)^2)
  mse[mc_iter, 5] <- mean((lasso_pred-test_splits[[mc_iter]]$DEXfat)^2)
  mse[mc_iter, 6] <- mean((mars_pred-test_splits[[mc_iter]]$DEXfat)^2)
  mse[mc_iter, 7] <- mean((gam_pred-test_splits[[mc_iter]]$DEXfat)^2)
  mse[mc_iter, 8] <- mean((stump_pred-test_splits[[mc_iter]]$DEXfat)^2)
  
  cat(paste("Done with iteration: ", mc_iter), sep = "\n")
  
}

mse_out <- apply(mse, 2, FUN = mean)
saveRDS(mse, file = "../99_Stored_Data/garcia_benchmark_results.rds")

# save selected boosting object for one iteration for further partial plots later
boost_objects <- list(gam1[m_stop_cvm], glm1[m_stop_aic])
saveRDS(boost_objects, file = "../99_Stored_Data/garcia_boost_objects.rds")

# par(mar=c(5, 4.1, 4.1, 6))

# plot(glm1, ylim = c(-0.5,6))

end.time <- Sys.time()
print(end.time - start.time)