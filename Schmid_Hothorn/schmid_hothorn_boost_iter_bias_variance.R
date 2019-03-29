rm(list = ls())

'
Simulates mse decomposition of model (11) in Schmid and Hothorn (2008)
'

library(mboost)

set.seed(1)

n_sim <- 100
n_boost <- 300
n_sample <- 100
p <- 9

prediction_matrix <- matrix(NA, nrow = n_sim, ncol = n_sample)
mse_temp <- matrix(NA, nrow = n_sim, ncol = n_boost)
results <- matrix(NA, nrow = 3, ncol = n_boost)


# sequence 0 to 3 (100 entries) to sample for train/test permutation designs  -------
x <- seq(from=0, to=3, length.out=n_sample)

# train data ------
x1 <- sample(x, size= n_sample)
x2_prep <- sample(x, size= n_sample)
x2 <- ifelse(x2_prep == 0, 1*10^(-1), x2_prep)
x3 <- sample(x, size= n_sample)
x4 <- sample(x, size= n_sample)
x5 <- sample(x, size= n_sample)
x6 <- sample(x, size= n_sample)
x7 <- sample(x, size= n_sample)
x8 <- sample(x, size= n_sample)
x9 <- sample(x, size= n_sample)

X <- matrix(c(x1, x2, x3, x4, x5, x6, x7, x8, x9), nrow = n_sample, ncol = p)

f_1 <- 1 + 8*sin(X[,1])
f_2 <- 3*log(X[,2])
f_3 <- -0.8*(X[,7]^4 - X[,7]^3 - 5*X[,7]^2)
f_4 <- -3*X[,8]

f_true <- f_1 + f_2 + f_3 + f_4
epsilon_train <- replicate(n_sim, rnorm(n_sample, 0, 3))
y <- replicate(n_sim, f_true) + epsilon_train

# test data -------
x1_test <- sample(x, size= n_sample)
x2_test_prep <- sample(x, size= n_sample)
x2_test <- ifelse(x2_test_prep == 0, 1*10^(-1), x2_test_prep)
x3_test <- sample(x, size= n_sample)
x4_test <- sample(x, size= n_sample)
x5_test <- sample(x, size= n_sample)
x6_test <- sample(x, size= n_sample)
x7_test <- sample(x, size= n_sample)
x8_test <- sample(x, size= n_sample)
x9_test <- sample(x, size= n_sample)

X_test <- matrix(c(x1_test, x2_test, x3_test, x4_test, x5_test, x6_test, x7_test, x8_test, x9_test), nrow = n_sample, ncol = p)

f_1_test <- 1 + 8*sin(X_test[,1])
f_2_test <- 3*log(X_test[,2])
f_3_test <- -0.8*(X_test[,7]^4 - X_test[,7]^3 - 5*X_test[,7]^2)
f_4_test <- -3*X_test[,8]

f_test_true <- f_1_test + f_2_test + f_3_test + f_4_test


### MC Simulation -----
for (boost_iter in seq(n_boost)){
  
  for (mc_iter in seq(n_sim)){
    
    boost_fit <- gamboost(y ~ .,
                          data = data.frame(y=y[,mc_iter],X), 
                          baselearner = "bbs",
                          dfbase = 3.5,
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

saveRDS(results, file = "schmid_hothorn_boost_iter_bias_variance.rds")
