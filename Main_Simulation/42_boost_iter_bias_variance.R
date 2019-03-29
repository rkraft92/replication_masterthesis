rm(list = ls())

set.seed(12)
library(mboost)
library(MASS)

n_boost = 500 
n_sim = 50
n_sample = 20
n_reg = 10
sd_eps = 2

prediction_matrix <- matrix(NA, nrow = n_sim, ncol = n_sample)
mse_temp <- matrix(NA, nrow = n_sim, ncol = n_boost)
results <- matrix(NA, nrow = 3, ncol = n_boost)

V <- diag(n_reg)
mu <- numeric(n_reg)

#### Create design matrix
X <- mvrnorm(n_sample, mu,V)
X_test <- mvrnorm(n_sample, mu,V)


##### Buhlmann High dimensional models
f_true <- 1*(1+5*X[,1]+2*X[,2]+X[,3])
f_test_true <- 1*(1+5*X_test[,1]+2*X_test[,2]+X_test[,3])

y <- replicate(n_sim, f_true) + replicate(n_sim, rnorm(n_sample, 0,sd_eps))


### MC Simulation -----
for (boost_iter in seq(n_boost)){
  
  for (mc_iter in seq(n_sim)){
    
    boost_fit <- glmboost(y ~ ., 
                          data = data.frame(y=y[,mc_iter],X), 
                          control = boost_control(mstop = n_boost,
                                                  nu = 0.1))
    
    boost_pred <- predict(boost_fit[boost_iter], data.frame(X_test))
    
    prediction_matrix[mc_iter,1:n_sample] <- boost_pred
    
    mse_temp[mc_iter,boost_iter] <- mean((boost_pred - f_test_true)^2)
  }
  
  var_matrix <- apply(prediction_matrix, 2, FUN = var)
  bias_matrix <- apply(prediction_matrix, 2, FUN = mean)
  
  squared_bias <- (bias_matrix - f_test_true)^2
  
  results[1, boost_iter] <- mean(var_matrix)
  results[2, boost_iter] <- mean(squared_bias)
  
}

results[3,1:n_boost] <- apply(mse_temp, 2, FUN = mean)

saveRDS(results, file = "../99_Stored_Data/42_boost_iter_bias_variance.rds")
