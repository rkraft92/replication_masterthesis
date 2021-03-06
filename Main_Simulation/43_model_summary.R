rm(list = ls())

'
This scripts replicates the results of Bühlmann: Boosting in high dimensions, p.567, model (4.3)
'


library(mboost)
library(lars)
library(MASS)
library(glmnet)
library(xtable)

# setwd("/Users/RobinKraft/Desktop/Master_Thesis/04_Code/R")
source('v_matrix.R')

results <- matrix(NA, nrow = 5, ncol = 3)
col_iter <- 1

for (p in c(3,10,100)){

set.seed(12)

n_sim = 50
n_sample = 20
n_reg = p
sd_eps = 2


### Create Variance-Covariance MAtrix and mean vector for multivariate 
### normal design matrix
b = 0.677 
c = 0.323

V <- v_matrix(n_reg = n_reg, b=b, c=c)
mu <- numeric(n_reg)

mse <- matrix(NA, nrow = n_sim, ncol= 5)

#### Create design matrix
X <- mvrnorm(n_sample, mu,V)
X_test <- mvrnorm(n_sample, mu,V)


##### Buhlmann High dimensional models
f_true <- 0.779*(1+5*X[,1]+2*X[,2]+X[,3])
f_test_true <- 0.779*(1+5*X_test[,1]+2*X_test[,2]+X_test[,3])

y <- replicate(n_sim, f_true) + replicate(n_sim, rnorm(n_sample, 0,sd_eps))

##### MC Simulation
for (k in seq(n_sim)){
  
  ##### Generating real valued response, true dgp + noise term
  # y <- f_true + rnorm(n_sample, 0,sd_eps)
  # y_test <- f_test_true + rnorm(n_sample, 0,sd_eps)
  
  ##### Model fitting
  # OLS
  ols_fit <- lm(y ~ ., data = data.frame(y=y[,k], X))
  
  #Fwd Var Selection
  fit_start <- lm(y~1, data = data.frame(y=y[,k],X))
  fit_end <- lm(y~., data = data.frame(y=y[,k],X))
  step_fit <- stepAIC(fit_start, direction="forward", scope=list(upper=fit_end,lower=fit_start), trace = F)
  
  #LASSO
  lasso_fit <- cv.glmnet(x=X, y=y[,k], alpha = 1, nfolds = 10)
  
  #L2Boosting
  boost_fit_initial <- glmboost(y ~ ., data = data.frame(y=y[,k],X), control = boost_control(mstop = 200,
                                                                                             nu = 0.1))
  m_stop_aic <- mstop(aic <- AIC(boost_fit_initial))
  m_stop_cv <- mstop(cvr <- cvrisk(boost_fit_initial))
  
  ##### Model prediction on X_test
  ols_pred <- predict(ols_fit, data.frame(X_test))
  step_pred <- predict(step_fit, data.frame(X_test))
  lasso_pred <-predict(lasso_fit, newx = X_test, s = "lambda.min")
  boost_pred_aic <- predict(boost_fit_initial[m_stop_aic], data.frame(X_test))
  boost_pred_cv <- predict(boost_fit_initial[m_stop_cv], data.frame(X_test))
  
  ##### Compute mse of respective model
  mse[k,1] <- mean((f_test_true-ols_pred)^2)
  mse[k,2] <- mean((f_test_true-step_pred)^2)
  mse[k,3] <- mean((f_test_true-lasso_pred)^2)
  mse[k,4] <- mean((f_test_true-boost_pred_aic)^2)
  mse[k,5] <- mean((f_test_true-boost_pred_cv)^2)
}

# print(paste('MSE OLS simulated', mean(mse[,1])))
# print(paste('MSE StepAIC simulated', mean(mse[,2])))
# print(paste('MSE LASSO simulated', mean(mse[,3])))
# print(paste('MSE L2Boosting simulated', mean(mse[,4])))

results[1,col_iter] <- mean(mse[,1])
results[2,col_iter] <- mean(mse[,2])
results[3,col_iter] <- mean(mse[,3])
results[4,col_iter] <- mean(mse[,4])
results[5,col_iter] <- mean(mse[,5])

col_iter <- col_iter +1
}

results <- as.data.frame(results)
colnames(results) <- c("p=3", "p=10", "p=100")
Method <- c('OLS', 'StepAIC','LASSO','L2Boosting', 'L2Boosting CV')
output <- cbind(Method, results)

print(xtable(output, digits = 4, type = "latex"), include.rownames = FALSE, file = "../../05_Text/Main_Simulation/model43_cv.tex")