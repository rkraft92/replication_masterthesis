rm(list = ls())

'
Returns distribution of mse for increasing degrees of freedom of the baselearner (smoothing spline).
The model is according to Schmid and Hothorn (2008), equation (11).
'

library(mboost)

set.seed(1)

n_sim <- 100
# n_boost <- 300
n <- 100
p <- 9

mse <- matrix(NA, nrow = n_sim, ncol = 8)

# sequence 0 to 3 (100 entries) to sample for train/test permutation designs  -------
x <- seq(from=0, to=3, length.out=n)

# train data ------
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

X <- matrix(c(x1, x2, x3, x4, x5, x6, x7, x8, x9), nrow = n, ncol = p)

f_1 <- 1 + 8*sin(X[,1])
f_2 <- 3*log(X[,2])
f_3 <- -0.8*(X[,7]^4 - X[,7]^3 - 5*X[,7]^2)
f_4 <- -3*X[,8]

f_true <- f_1 + f_2 + f_3 + f_4
epsilon_train <- replicate(n_sim, rnorm(n, 0, 3))
y <- replicate(n_sim, f_true) + epsilon_train

# test data -------
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

X_test <- matrix(c(x1_test, x2_test, x3_test, x4_test, x5_test, x6_test, x7_test, x8_test, x9_test), nrow = n, ncol = p)

f_1_test <- 1 + 8*sin(X_test[,1])
f_2_test <- 3*log(X_test[,2])
f_3_test <- -0.8*(X_test[,7]^4 - X_test[,7]^3 - 5*X_test[,7]^2)
f_4_test <- -3*X_test[,8]

f_true_test <- f_1_test + f_2_test + f_3_test + f_4_test

col_df_iter <- 1
for (df_iter in c(2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)){
  for (mc_iter in seq(n_sim)){
    
    boost_model <- gamboost(y ~ .,
                            data = data.frame(y=y[, mc_iter], X), 
                            baselearner = "bbs",
                            dfbase = df_iter,
                            control = boost_control(mstop = 1000, nu = 0.1))
    m_stop <- mstop(aic <- AIC(boost_model))
    # m_stop <- mstop( cvm <- cvrisk(boost_model))
    # m_stop <- 100
    boost_pred <- predict(boost_model[m_stop], newdata = data.frame(X_test))
    
    mse[mc_iter, col_df_iter] <- mean((boost_pred - f_true_test)^2)
    
  }
  
  col_df_iter <- col_df_iter + 1 
  cat(paste0("Done with iteration ", col_df_iter, "\n"))
}

saveRDS(mse, file = "schmid_hothorn_df_dev.rds")

mse_out <- apply(mse, 2, mean)
# boxplot(mse, use.cols = TRUE)
# changed outlier to 0.1