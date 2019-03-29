rm(list = ls())

set.seed(123)

n_sim <- 200
n_df <- 40
n_sample <- 100


prediction_matrix <- matrix(NA, nrow = n_sim, ncol = n_sample)
results <- matrix(NA, nrow = 2, ncol = n_df)

# Train data -----
x_train <- runif(n_sample, -0.5, 0.5)
f_train <- 0.8*x_train+sin(6*x_train)

epsilon_train <- replicate(n_sim, rnorm(n_sample,0,sqrt(2)))
y_train <- replicate(n_sim,f_train) + epsilon_train

# Test data -----
x_test <- runif(n_sample, -0.5, 0.5)
f_test <- 0.8*x_test+sin(6*x_test)


for (df_iter in seq(n_df)){
  
  for (mc_iter in seq(n_sim)){
    cspline <- smooth.spline(x_train, y_train[,mc_iter], df=df_iter+1)
    
    cspline_predict <- predict(cspline, x_test)
    
    prediction_matrix[mc_iter,1:n_sample] <- cspline_predict$y 
    
  }
  
  var_matrix <- apply(prediction_matrix, 2, FUN = var)
  bias_matrix <- apply(prediction_matrix, 2, FUN = mean)
  
  squared_bias <- (bias_matrix - f_test)^2
  
  results[1, df_iter] <- mean(var_matrix)
  results[2, df_iter] <- mean(squared_bias)
  
}

saveRDS(results, file = "../99_Stored_Data/toy_cs_spline_bias_variance.rds")

# par(mfrow=c(1,2))
# plot(results[1,], type = "l", xlab = "Degrees of Freedom", ylab = "Variance")
# plot(results[2,], type = "l", xlab = "Degrees of Freedom", ylab = "Squared-Bias")