rm(list = ls())
set.seed(123)

library(mboost)

n_sim <- 200
n_boost <- 200
n_sample <- 100


prediction_matrix <- matrix(NA, nrow = n_sim, ncol = n_sample)
results <- matrix(NA, nrow = 2, ncol = n_boost)


# Train data -----
x_train <- runif(n_sample, -0.5, 0.5)
f_train <- 0.8*x_train+sin(6*x_train)

epsilon_train <- replicate(n_sim, rnorm(n_sample,0,sqrt(2)))
y_train <- replicate(n_sim,f_train) + epsilon_train

# Test data -----
x_test <- runif(n_sample, -0.5, 0.5)
f_test <- 0.8*x_test+sin(6*x_test)

# MC Simulation ----
for (boost_iter in seq(n_boost)){
  
  for (mc_iter in seq(n_sim)){
    boost <- gamboost(y ~ x,
                      data = data.frame(y=y_train[,mc_iter],x=x_train),
                      dfbase = 3,
                      control = boost_control(mstop = n_boost,
                                              nu = 1))
    
    boost_predict <- predict(boost[boost_iter], type = 'response', newdata = data.frame(x=x_test))
    
    prediction_matrix[mc_iter,1:n_sample] <- boost_predict
    
  }
  
  var_matrix <- apply(prediction_matrix, 2, FUN = var)
  bias_matrix <- apply(prediction_matrix, 2, FUN = mean)
  
  squared_bias <- (bias_matrix - f_test)^2
  
  results[1, boost_iter] <- mean(var_matrix)
  results[2, boost_iter] <- mean(squared_bias)
  
}

saveRDS(results, file = "../99_Stored_Data/toy_boosting_iterations_bias_variance.rds")

# par(mfrow=c(1,2))
# plot(results[1,], type = "l", xlab = "Degrees of Freedom", ylab = "Variance")
# plot(results[2,], type = "l", xlab = "Degrees of Freedom", ylab = "Squared-Bias")
