rm(list = ls())
set.seed(123)

library(mboost)

n_sim <- 200
n_boost <- 200
n_sample <- 100

mse_temp <- matrix(NA, nrow=n_sim, ncol=n_boost)

# Train data -----
x_train <- runif(n_sample, -0.5, 0.5)
f_train <- 0.8*x_train+sin(6*x_train)

epsilon_train <- replicate(n_sim, rnorm(n_sample,0,sqrt(2)))
y_train <- replicate(n_sim,f_train) + epsilon_train

# Test data -----
x_test <- runif(n_sample, -0.5, 0.5)
f_test <- 0.8*x_test+sin(6*x_test)

# MC Simulation ----
for (mc_iter in seq(n_sim)){
  # mc_iter <- 1
  boost <- gamboost(y ~ x,
                    data = data.frame(y=y_train[,mc_iter],x=x_train),
                    dfbase = 3,
                    control = boost_control(mstop = n_boost,
                                            nu = 1))
  
  for (boost_iter in seq(n_boost)){
    # boost_iter <- 1
    boost_predict <- predict(boost[boost_iter], type = 'response', newdata = data.frame(x=x_test))
    
    mse_temp[mc_iter, boost_iter] <- mean((f_test - boost_predict)^2)
    
  }
}

mse <- apply(mse_temp, 2, FUN = mean)

saveRDS(mse, file = "../99_Stored_Data/toy_boosting_iterations_mse.rds")
# plot(seq(n_boost),mse, type = 'l')
