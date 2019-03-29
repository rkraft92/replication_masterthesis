rm(list = ls())

library(mboost)
library(earth)
library(gam)
library(xtable)


### container for results
results <- matrix(NA, nrow = 4, ncol = 2)
col_iter <- 1


for (p in c(9, 50)){
  
  set.seed(1)  
  n_sim <- 100
  n <- 100
  
  ### container for mse simulation
  mse <- matrix(NA, nrow=n_sim, ncol = 4)
  
  # sequence 0 to 3 (100 entries) to sample for train/test permutation designs  -------
  x <- seq(from=0, to=3, length.out=n)
  
  ###################################
  #### GENERATING TRAININGS DATA ####
  ###################################
  # train data for p = 9 ------
  x1 <- sample(x, size= n)
  x2_prep <- sample(x, size= n)
  x2 <- ifelse(x2_prep == 0, 1*10^(-1), x2_prep)
  x3 <- sample(x, size= n)
  x4 <- sample(x, size= n)
  x5 <- sample(x, size= n)
  x6 <- sample(x, size= n)
  x7 <- sample(x, size= n)
  x8 <- sample(x, size= n)
  x9 <- sample(x, size= n)
  
  # additional noise for p = 50
  if (p == 50){
    X_add <- replicate(p-9, runif(n, 0, 1))
  }else{
    X_add <- NULL
  }
  
  # actual trainings design matrix
  X_train <- matrix(c(x1, x2, x3, x4, x5, x6, x7, x8, x9, X_add), nrow = n, ncol = p)
  
  f_1 <- 1 + 8*sin(X_train[,1])
  f_2 <- 3*log(X_train[,2])
  f_3 <- -0.8*(X_train[,7]^4 - X_train[,7]^3 - 5*X_train[,7]^2)
  f_4 <- -3*X_train[,8]
  
  f_true <- f_1 + f_2 + f_3 + f_4
  epsilon_train <- replicate(n_sim, rnorm(n, 0, 3))
  y <- replicate(n_sim, f_true) + epsilon_train
  
  ###########################
  #### GENERATING TEST DATA #
  ###########################
  # test data for p = 9 -------
  x1_test <- sample(x, size= n)
  x2_test_prep <- sample(x, size= n)
  x2_test <- ifelse(x2_test_prep == 0, 1*10^(-1), x2_test_prep)
  x3_test <- sample(x, size= n)
  x4_test <- sample(x, size= n)
  x5_test <- sample(x, size= n)
  x6_test <- sample(x, size= n)
  x7_test <- sample(x, size= n)
  x8_test <- sample(x, size= n)
  x9_test <- sample(x, size= n)
  
  # addtional noise for p = 50
  if (p == 50){
    X_add_test <- replicate(p-9, runif(n, 0, 1))
  } else{
    X_add_test <- NULL
  }
  
  # actual test design matrix
  X_test <- matrix(c(x1_test, x2_test, x3_test, x4_test, x5_test, x6_test, x7_test, x8_test, x9_test, X_add_test), nrow = n, ncol = p)
  
  f_1_test <- 1 + 8*sin(X_test[,1])
  f_2_test <- 3*log(X_test[,2])
  f_3_test <- -0.8*(X_test[,7]^4 - X_test[,7]^3 - 5*X_test[,7]^2)
  f_4_test <- -3*X_test[,8]
  
  f_true_test <- f_1_test + f_2_test + f_3_test + f_4_test
  
  ##########################
  ##### MC SIMULATION ------
  ##########################
  for (mc_iter in seq(n_sim)){ # mc_iter <- 1
    # cubic smoothing spline as base-learner by default
    
    #### Boosting fit
    boost_model <- gamboost(y ~ ., 
                            data = data.frame(y=y[,mc_iter],X_train),
                            baselearner = "bbs",
                            dfbase = 3.5,
                            control = boost_control(mstop = 300, nu = 0.1))
    m_stop_boost <- mstop(aic <- AIC(boost_model))
    
    #### Stump fit (btree = stump per default)
    stump_fit <- gamboost(y ~ ., 
                          data = data.frame(y=y[,mc_iter],X_train), 
                          baselearner = "btree",
                          control = boost_control(mstop = 300, nu = 0.1))
    m_stop_stump <- mstop(cvr <- cvrisk(stump_fit))
    
    #### MARS fit
    mars_fit <- earth(y ~ .,  data = data.frame(y=y[,mc_iter],X_train))  
    
    #### GAM fit with inclusion strategy
    gam_fit <- gam(y ~ 1, family = "gaussian", data = data.frame(y=y[,mc_iter],X_train))
    gam_scope <- gam.scope(frame = data.frame(y=y[,mc_iter],X_train), response = 1, arg = "df=5")
    gam_step <- step.gam(gam_fit, gam_scope, direction = "forward", trace = FALSE)
    
    #### Predictions
    boost_pred <- predict(boost_model[m_stop_boost], data.frame(X_test))
    stump_pred <- predict(stump_fit[m_stop_stump], data.frame(X_test))
    mars_pred <- predict(mars_fit, data.frame(X_test))
    gam_pred <- predict(gam_step, data.frame(X_test))
    
    ### MSE calculation
    mse[mc_iter,1] <- mean((f_true_test-boost_pred)^2)
    mse[mc_iter,2] <- mean((f_true_test-stump_pred)^2)
    mse[mc_iter,3] <- mean((f_true_test-mars_pred)^2)
    mse[mc_iter,4] <- mean((f_true_test-gam_pred)^2)
  }
  
  results[1:4, col_iter] <- apply(mse, 2, FUN = mean)
  
  col_iter <- col_iter +1
}

results <- as.data.frame(results)
colnames(results) <- c("p=9", "p=50")
Method <- c('L2Boost with componentwise spline', 'L2Boost with stumps','MARS','additive model (backfitted)')
output <- cbind(Method, results)

print(xtable(output, digits = 4, type = "latex"), include.rownames = FALSE, file = "../../05_Text/Main_Simulation/schmid_hothorn_results.tex")