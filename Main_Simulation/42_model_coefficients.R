rm(list = ls())

'
This script simulates model (4.2) of Bühlmann: Boosting in high dimensions, p. 567.
'
library(mboost)
library(MASS)
library(xtable)
library(tikzDevice)

set.seed(12)

n_sim = 50
n_sample = 20
n_reg = 10
sd_eps = 2

mse <- matrix(NA, nrow = n_sim, ncol= 1)

V <- diag(n_reg)
mu <- numeric(n_reg)


#### Create design matrix ------
X <- mvrnorm(n_sample, mu,V)
X_test <- mvrnorm(n_sample, mu,V)


##### Buhlmann High dimensional models ------
f_true <- 1*(1+5*X[,1]+2*X[,2]+X[,3])
f_test_true <- 1*(1+5*X_test[,1]+2*X_test[,2]+X_test[,3])

y <- replicate(n_sim, f_true) + replicate(n_sim, rnorm(n_sample, 0,sd_eps))


#### L2Boosting for first generated train data --------
k <- 1
boost_fit <- glmboost(y ~ ., data = data.frame(y=y[,k],X), control = boost_control(mstop = 500, nu = 0.1))
m_stop_aic <- mstop(aic <- AIC(boost_fit, method ='corrected'))

### Coefficient Plot for one selected Monte Carlo iteration -----
# Interpretation: the higher the complexity of the functional form f_j, 
# the higher the regreesion coefficient (direct relation to degrees of freedom as shown in theory)
tikz("../../06_Plots/Main_Simulation/42_model_coefficient_path.tex", width = 6, height=4)
par(mar = c(4,4,1,5))
plot(boost_fit[m_stop_aic], main = "")
dev.off()
