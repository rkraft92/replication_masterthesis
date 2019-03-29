rm(list = ls())

'
This scripts replicates the results of Bühlmann: Boosting in high dimensions, p.567, model (4.3)
'


library(mboost)
library(lars)
library(MASS)
library(glmnet)
library(xtable)

source('v_matrix.R')


set.seed(12)

n_sim = 50
n_sample = 20
n_reg = 10
sd_eps = 2


### Create Variance-Covariance MAtrix and mean vector for multivariate 
### normal design matrix
b = 0.677 
c = 0.323

V <- v_matrix(n_reg = n_reg, b=b, c=c)
mu <- numeric(n_reg)


#### Create design matrix
X <- mvrnorm(n_sample, mu,V)
X_test <- mvrnorm(n_sample, mu,V)


##### Buhlmann High dimensional models
f_true <- 0.779*(1+5*X[,1]+2*X[,2]+X[,3])
f_test_true <- 0.779*(1+5*X_test[,1]+2*X_test[,2]+X_test[,3])

y <- replicate(n_sim, f_true) + replicate(n_sim, rnorm(n_sample, 0,sd_eps))

#### L2Boosting for first generated train data --------
k <- 1
boost_fit_initial <- glmboost(y ~ ., data = data.frame(y=y[,k],X), control = boost_control(mstop = 500, nu = 0.1))
m_stop_aic <- mstop(aic <- AIC(boost_fit_initial))

### Coefficient Plot for one selected Monte Carlo iteration -----
# Interpretation: the higher the complexity of the functional form f_j, 
# the higher the regreesion coefficient (direct relation to degrees of freedom as shown in theory)
# variance of X3 (0.799^2) less than variance of X_j for j >=4 (1^2)
pdf("../../06_Plots/Main_Simulation/43_model_coefficient_path.pdf", width = 6, height=4)
par(mar = c(4,4,1,5))
plot(boost_fit_initial[m_stop_aic], main = "")
dev.off()  
