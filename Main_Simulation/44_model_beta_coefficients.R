rm(list = ls())

library(MASS)
library(tikzDevice)
library(latex2exp)

set.seed(123)



simulate_data <- function(n, p, kappa){
  
  
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
  
  
  # True data generating process
  mu_vector <- numeric(p)
  covariance_matrix <- diag(p)
  
  X_design <- mvrnorm(n, mu = mu_vector, Sigma = covariance_matrix)
  
  f_true <- numeric(n)
  for (j in seq(p)){
    
    f_true <- f_true + beta[j]*X_design[,j]
    
  }
  
  return(beta)
  
}


n <- 100
p <- 23
kappa <- 0.199

# calculation by wolfram alpha sum j^0.51*max(1-0.15*j^0.51,0), j=1 to infinity
k_test <- 0.15
conv_ser <- 45.7982
n_new <- conv_ser/k_test
p_new <- exp(-log(k_test)*(1/0.51))

# 
# k_test2 <- 0.12
# conv_ser2 <- 88.8431
# n_new2 <- conv_ser2/k_test2
# p_new2 <- exp(-log(k_test2)*(1/0.51))

# Train and Test Data
train_data <- simulate_data(n=n, p=p, kappa = kappa)
train_data1 <- simulate_data(n=ceiling(n_new), p=ceiling(p_new), kappa = k_test)
# train_data2 <- simulate_data(n=ceiling(n_new2), p=ceiling(p_new2), kappa = k_test2)

# tikz('../../06_Plots/Main_Simulation/44_beta_coefficients.tex', width = 6, height=4)
pdf('../../06_Plots/Main_Simulation/44_beta_coefficients.pdf', width = 6, height=4)
par(mfrow = c(1,2))
plot(abs(train_data), xlab = TeX("\\textit{j}"), ylab = TeX("$|\\beta_j|$"))
plot(abs(train_data1), xlab = TeX("\\textit{j}"), ylab = "" , ylim = c(0,max(abs(train_data))))#, yaxt='n')
# plot(abs(train_data2))
dev.off()


### GGPLOT Implementation ------
library(ggplot2)
library(gridExtra)
pl1 <- ggplot(data.frame(x=seq(abs(train_data)),y=abs(train_data)), aes(y=y, x=x)) +
  geom_point()+
  xlab(TeX("\\textit{j}"))+
  ylab(TeX("$|\\beta_j|$"))+
  ylim(0, max(abs(train_data)))+
  theme_bw()+
  theme(panel.grid = element_blank())

pl2 <- ggplot(data.frame(x=seq(abs(train_data1)),y=abs(train_data1)), aes(y=y, x=x)) +
  geom_point()+
  xlab(TeX("\\textit{j}"))+
  ylab(TeX(""))+
  ylim(0, max(abs(train_data)))+
  theme_bw()+
  theme(panel.grid = element_blank())

pdf('../../06_Plots/Main_Simulation/44_beta_coefficients.pdf', width = 6, height=3)
grid.arrange(pl1, pl2, ncol = 2)
dev.off()
