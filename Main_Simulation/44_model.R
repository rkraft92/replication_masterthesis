rm(list = ls())

'
This code replicates the results of Bühlmann: Boosting in high dimensions, p.570, Table 4, model (4.4)
'

library(MASS)
library(mboost)
library(glmnet)
library(xtable)

set.seed(1)

### Function for simulating beta coefficients; 
# those are fixed for the whole simulation study
beta_sim <- function(n,p,kappa){

  beta <- numeric(p)
  sigma2 <- numeric(p)
  
  # Generate beta coefficients
  for (j in seq(p)){
    
    a_j <- j^0.51
    lambda_j <- ifelse(1-kappa*a_j>0,1-kappa*a_j,0)
    sigma2_j <- lambda_j*(n*kappa*a_j)^(-1)
    
    sigma2[j] <- sigma2_j
    
    beta[j] <- rnorm(1, 0,sqrt(sigma2_j))
    lambda_j <- NULL
    
  }
  return(beta)
}

### Function for simulating data
simulate_data <- function(n, p, kappa, beta){
  
  # True data generating process
  mu_vector <- numeric(p)
  covariance_matrix <- diag(p)
  
  X_design <- mvrnorm(n, mu = mu_vector, Sigma = covariance_matrix)
  
  f_true <- numeric(n)
  for (j in seq(p)){
    
    f_true <- f_true + beta[j]*X_design[,j]
    
  }
  
  return(list(f_true, X_design))

}

########################################################
#### Actual script starts ------------------------------
########################################################

# Simulation set-up
n_sim <- 50
n <- 100
p <- 23
kappa <- 0.199

mse <- matrix(NA, nrow = n_sim, ncol = 4) # numeric(n_sim)

# Simulate beta coefficients
beta_simulated <- beta_sim(n=n, p=p, kappa=kappa)

# Train and Test Data
train_data <- simulate_data(n=n, p=p, kappa = kappa, beta=beta_simulated)
f_train <- train_data[[1]]
X_train <- train_data[[2]]

test_data <- simulate_data(n=n, p=p, kappa = kappa, beta=beta_simulated)
f_test <- test_data[[1]]
X_test <- test_data[[2]]


# MC Simulation
epsilon_train <- replicate(n_sim, rnorm(n, 0, 1))
y_train <- replicate(n_sim, f_train) + epsilon_train


for (idx in seq(n_sim)){
  # idx <- 1
  # OLS
  ols_fit <- lm(y ~ ., data = data.frame(y = y_train[, idx], X_train))
  
  #AIC
  fit_start <- lm(y~1, data = data.frame(y = y_train[, idx],X_train))
  fit_end <- lm(y~., data = data.frame(y=y_train[, idx],X_train))
  step_fit <- stepAIC(fit_start, direction="forward", scope=list(lower=fit_start, upper=fit_end), trace = F)
  
  #LASSO
  lasso_fit <- cv.glmnet(x=X_train, y=y_train[,idx], alpha = 1, nfolds = 10)
  
  # Boosting 
  boost_initial <- glmboost(X_train, 
                    y_train[,idx],
                    center=FALSE,
                    control = boost_control(mstop = 100, 
                                            nu = 0.1))
  m_stop <- mstop(aic <- AIC(boost_initial))

  # Prediction out-of-sample
  ols_pred <- predict(ols_fit, data.frame(X_test))
  step_pred <- predict(step_fit, data.frame(X_test))
  lasso_pred <-predict(lasso_fit, newx = X_test, s = "lambda.min")
  boost_predict <- predict(boost_initial[m_stop], X_test)
  
  mse[idx, 1] <- mean((f_test - ols_pred)^2)
  mse[idx, 2] <- mean((f_test - step_pred)^2)
  mse[idx, 3] <- mean((f_test - lasso_pred)^2)
  mse[idx, 4] <- mean((f_test - boost_predict)^2)
  
}

print(paste("MSE OLS", mean(mse[,1])))
print(paste("MSE StepAIC",mean(mse[,2])))
print(paste("MSE LASSO", mean(mse[,3])))
print(paste("MSE L2Boosting",mean(mse[,4])))


### Results to LATEX table -----
results <- matrix(NA, nrow = 1, ncol = 4)
results <- as.data.frame(results)
results[1,1] <- mean(mse[,1])
results[1,2] <- mean(mse[,2])
results[1,3] <- mean(mse[,3])
results[1,4] <- mean(mse[,4])

colnames(results) <- c('OLS', 'StepAIC', 'LASSO', 'L2Boosting')
print(xtable(results, digits = 4, type = "latex"), include.rownames = FALSE, file = "../../05_Text/Main_Simulation/model44.tex")