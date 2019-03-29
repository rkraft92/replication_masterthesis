rm(list = ls())

'
Generates one distinct boosting model 
and shows higher/lower order adaption of smoothing spline base-procedure.
In particular: comparison of fitted values vs true values plot for each selected covariate
'

library(mboost)
library(latex2exp)


set.seed(1)  
n_sim <- 100
n <- 100
p <- 9
  
### container for mse simulation
mse <- matrix(NA, nrow=n_sim, ncol = 4)
  
# sequence 0 to 3 (100 entries) to sample for train/test permutation designs  -------
x <- seq(from=0, to=3, length.out=n)
  
###################################
#### GENERATING TRAININGS DATA ####
###################################
# train data for p = 9 ------
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
  
# actual trainings design matrix
X_train <- matrix(c(x1, x2, x3, x4, x5, x6, x7, x8, x9), nrow = n, ncol = p)
  
f_1 <- 8*sin(X_train[,1])
f_2 <- 3*log(X_train[,2])
f_3 <- -0.8*(X_train[,7]^4 - X_train[,7]^3 - 5*X_train[,7]^2)
f_4 <- -3*X_train[,8]
  
f_true <- 1 + f_1 + f_2 + f_3 + f_4
epsilon_train <- replicate(n_sim, rnorm(n, 0, 3))
y <- replicate(n_sim, f_true) + epsilon_train


### fit boosting model ---------  
mc_iter <- 38  
boost_model <- gamboost(y ~ ., 
                        data = data.frame(y=y[,mc_iter],X_train),
                        baselearner = "bbs",
                        dfbase = 3.5,
                        control = boost_control(mstop = 500, nu = 0.1))

m_stop <- mstop(aic <- AIC(boost_model))

### fitted values boosting ------
x1_fit <- fitted(boost_model[m_stop], which = 1)
x2_fit <- fitted(boost_model[m_stop], which = 2)
x3_fit <- fitted(boost_model[m_stop], which = 3)
x4_fit <- fitted(boost_model[m_stop], which = 4)
x5_fit <- fitted(boost_model[m_stop], which = 5)
x6_fit <- fitted(boost_model[m_stop], which = 6)
x7_fit <- fitted(boost_model[m_stop], which = 7)
x8_fit <- fitted(boost_model[m_stop], which = 8)
x9_fit <- fitted(boost_model[m_stop], which = 9)

#### true function values -------
f_1_true <- f_1 - mean(f_1)
f_2_true <- f_2 - mean(f_2)
f_3_true <- f_3 - mean(f_3)
f_4_true <- f_4 - mean(f_4)

### dataframes for plotting -----
pl1 <- data.frame(x1, f_1_true)
pl1 <- pl1[order(x1),]

pl2 <- data.frame(x2, f_2_true)
pl2 <- pl2[order(x2),]

pl7 <- data.frame(x7, f_3_true)
pl7 <- pl7[order(x7),]

pl8 <- data.frame(x8, f_4_true)
pl8 <- pl8[order(x8),]

#### plots -----------
pdf("../../06_Plots/Main_Simulation/schmid_hothorn_higher_order_adaption.pdf")
par(mfrow = c(3,3))
plot(pl1$x1, pl1$f_1_true, type = "l", ylim = c(-5,5), xlab = TeX("X^{(1)}"), ylab = expression(f[1]))
lines(x1, x1_fit, type = "p")

plot(pl2$x2, pl2$f_2_true, type = "l", ylim = c(-5,5), xlab = TeX("X^{(2)}"), ylab = expression(f[2]))
lines(x2, x2_fit, type = "p")

plot(x3, x3_fit, type = "p", ylim = c(-5,5), xlab = TeX("X^{(3)}"), ylab = expression(f[3]))
abline(h = 0)

plot(x4, x4_fit, type = "p", ylim = c(-5,5), xlab = TeX("X^{(4)}"), ylab = expression(f[4]))
abline(h = 0)

plot(x5, x5_fit, type = "p", ylim = c(-5,5), xlab = TeX("X^{(5)}"), ylab = expression(f[5]))
abline(h = 0)

plot(x6, replicate(n, 0), type = "p", ylim = c(-5,5), xlab = TeX("X^{(6)}"), ylab = expression(f[6]))
abline(h = 0)

plot(pl7$x7, pl7$f_3_true, type = "l", ylim = c(-5,5), xlab = TeX("X^{(7)}"), ylab = expression(f[7]))
lines(x7, x7_fit, type = "p")

plot(pl8$x8, pl8$f_4_true, type = "l", ylim = c(-5,5), xlab = TeX("X^{(8)}"), ylab = expression(f[8]))
lines(x8, x8_fit, type = "p")

plot(x9, x9_fit, type = "p", ylim = c(-5,5), xlab = TeX("X^{(9)}"), ylab = expression(f[9]))
abline(h = 0)
dev.off()