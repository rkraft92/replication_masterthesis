rm(list = ls())

set.seed(123)

n_sim <- 200
n_df <- 40
n_sample <- 100

mse_temp <- matrix(NA, nrow = n_sim, ncol = n_df)

# Train data -----
x_train <- runif(n_sample, -0.5, 0.5)
f_train <- 0.8*x_train+sin(6*x_train)

epsilon_train <- replicate(n_sim, rnorm(n_sample,0,sqrt(2)))
y_train <- replicate(n_sim,f_train) + epsilon_train

# Test data -----
x_test <- runif(n_sample, -0.5, 0.5)
f_test <- 0.8*x_test+sin(6*x_test)


for (mc_iter in seq(n_sim)){
  
  for (df_iter in seq(n_df)){
    cspline <- smooth.spline(x_train, y_train[,mc_iter], df=df_iter+1)
    
    cspline_predict <- predict(cspline, x_test)
    
    mse_temp[mc_iter, df_iter] <- mean((f_test - cspline_predict$y)^2)
    
  }
}

mse_spline <- apply(mse_temp, 2, FUN = mean)

saveRDS(mse_spline, file = "../99_Stored_Data/toy_cs_spline_df_mse.rds")
