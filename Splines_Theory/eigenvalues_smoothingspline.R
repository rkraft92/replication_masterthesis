rm(list=ls())
set.seed(1234)

n= 150
#x= runif(n, -5, 10); 
x = runif(n,-0.2,0.2)
x= sort(x)
#y= 1 + .1*x^2 - .1*x^4 + .01*x^5 + rnorm(n, 0, 20);
y <- (0.5 - 0.9* exp(-50*x^2))*x + 0.02 *rnorm(n)
f_true <- (0.5 - 0.9* exp(-50*x^2))*x

df = 4
fit= smooth.spline(x, y, df = df)
plot(x, y, las = 1, main = 'Smoothing Spline with df = 4')
#lines(x, 1 + .1*x^2 - .1*x^4 + .01*x^5, lwd=2, col="darkgoldenrod1")
lines(x, f_true, lwd=3, lty = 2)
lines(fit, col="steelblue3", lwd=2)
#legend(6.5, 40, c("true dgp", "smooth.spline function "), lty=c(1,2), 
#       cex=0.7, col=c("darkgoldenrod1","steelblue3"))

legend(0.12, 0.555, c("true dgp", "smooth.spline function "), lty=c(1,2), 
       cex=0.7, col=c("darkgoldenrod1","steelblue3"))


# Value Second derivative representation of natural cubic splines (Green & Silverman)
d = diff(x)
R = matrix(0, n-2, n-2)
for (j in 2:(n-2)){
  R[j-1,j-1] = (d[j-1]+d[j])/3
  R[j-1,j] = d[j]/6
  R[j,j-1] = d[j]/6
}
R[n-2, n-2] = (d[n-2]+d[n-1])/3

Q = matrix(0, n, n-2)
for (j in 2:(n-1)){
  Q[j-1,j-1] = 1/d[j-1]
  Q[j,j-1] = -(1/d[j-1]+1/d[j])
  Q[j+1,j-1] = 1/d[j]
}

lambda= 0.001;
K = Q%*%solve(R)%*%t(Q);
S1 = solve(diag(n)+lambda*K)

yh1 = S1%*%y

lambda=0.01;
S1000= solve(diag(n)+lambda*K)
yhl1000=S1000%*%y #S*y fitted values

lambda=0.000001;
S_low= solve(diag(n)+lambda*K)
yhlow=S_low%*%y #S*y fitted values


pdf('../../06_Plots/Splines_Theory/spline_lambda.pdf', width = 6, height=4)
plot(x, y, las = 1)
lines(x, f_true, lwd=2, lty = 2)
lines(x, yh1, lwd=2, col = 2)
lines(x, yhl1000, lwd=2, col = 3)
lines(x, yhlow, lwd=2, col = 4)
dev.off()

# Least Squares OLS Projection Matrix
X= cbind(rep(1,n), x)
H= X%*%solve(t(X)%*%X)%*%t(X)
yhls= H%*%y


# Lambda=1 eigenvalues
eeS1= eigen(S1)
eval1= eeS1$values

# Lambda = 100 eigenvalues
eeS1000= eigen(S1000)
eval1000= eeS1000$values

# OLS projection matrix
eeH= eigen(H)
evalH= eeH$values

# plot first 10 eigenvalues
pdf('../../06_Plots/Splines_Theory/spline_eigenvalues.pdf', width = 6, height=4)
par(mfrow=c(1,3), mar=c(2,2,2,1)) 
plot(eval1[1:10], pch=19, cex=1.5, main="Smoother Matrix - lambda=0.001")
plot(eval1000[1:10], pch=19, cex=1.5, 
     main="Smoother Matrix - lambda=0.01")
plot(evalH[1:10], pch=19, cex=1.5, main="Projection Matrix")
dev.off()

# Eigenvectors
# Interpretation: the smaller the eigenvalue of K, the smoother the eigenvector
# if lambda large, eigenvalue of S decreases and smoothing spline places less weight on "nonsmooth" eigenvalue
# no penalty on first two eigenvalues of S
evec1= eeS1$vectors
evec1000= eeS1000$vectors

pdf('../../06_Plots/Splines_Theory/spline_eigenvectors.pdf', width = 6, height=4)
par(mfrow=c(1,4), mar=c(1,2,1,1))
plot(x, evec1[,2], type="l", lwd=3)
plot(x, evec1[,4], type="l", lwd=3)
plot(x, evec1[,6], type="l", lwd=3)
plot(x, evec1[,8], type="l", lwd=3)
dev.off()

plot(x, evec1000[,2], type="l", lwd=3)
plot(x, evec1000[,4], type="l", lwd=3)
plot(x, evec1000[,8], type="l", lwd=3)
plot(x, evec1000[,10], type="l", lwd=3)

##### Boosting Operator Eigenvalues -----
library(expm)

eigen_boost <- function(m, hat_mat){
# boosting iteraions
A <- diag(n)-hat_mat

# boosting operator implementation as in Bühlmann/Yu
boost_operator <- diag(n) - A%^%(m+1)
# eigenvalues of boosting operator
eval_boost <- eigen(boost_operator)$values

return(eval_boost)
}

hat_mat <- S1000

pdf('../../06_Plots/Boosting_Theory/eigenvalues_boosting.pdf')
par(mfrow = c(2,2))
plot(eigen_boost(m=30, hat_mat=hat_mat)[1:10], pch=19, cex=1.5, main="m = 5", xlab = "", ylab = "")
plot(eigen_boost(m=50, hat_mat=hat_mat)[1:10], pch=19, cex=1.5, main="m = 30", xlab = "", ylab = "")
plot(eigen_boost(m=110, hat_mat=hat_mat)[1:10], pch=19, cex=1.5, main="m= 50", xlab = "", ylab = "")
plot(eigen_boost(m=500, hat_mat=hat_mat)[1:10], pch=19, cex=1.5, main="m = 110", xlab = "", ylab = "")
dev.off()



###### GGPLOT2 IMPLEMENTATION -------
library(ggplot2)
library(gridExtra)
library(dplyr)
library(latex2exp)

# Lambda plot ---> check labels (TEX) and linetypes
lambda_plot_df <- data.frame(x, y, f_true, yh1, yhl1000, yhlow)
alpha = c("0.001", "0.01", "0")
lambda_plot <- lambda_plot_df %>%
  ggplot()+
  geom_point(aes(x=x, y = y), shape = 21)+
  geom_line(aes(x=x, y = f_true, linetype = "true"), size = 0.75)+
  geom_line(aes(x=x, y=yh1, color = "yh1"))+
  geom_line(aes(x=x, y=yhl1000, color = "yhl1000"))+
  geom_line(aes(x=x, y=yhlow, color = "yhlow"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="bottom")+
  scale_linetype_manual(name = "",
                        values=c(2),
                        labels=c("true"))+
  scale_color_manual(name = "", values = c("red", "blue", "green"), labels=lapply(ifelse(alpha == "0",
                                                                                         sprintf('$\\lambda \\rightarrow %s$', alpha),
                                                                                         sprintf('$\\lambda = %s$', alpha)), TeX)
                                                                                  )
ggsave(lambda_plot, file = '../../06_Plots/Splines_Theory/spline_lambda.pdf', width = 5, height=4)

# Eigenvalue plot
eigenvalues <- data.frame(x = seq(10), e1 = eval1[1:10], e1000 = eval1000[1:10], eP = evalH[1:10])

eigen1 <- eigenvalues %>%
  ggplot()+
  geom_point(aes(x=x, y=e1))+
  scale_x_continuous(breaks=seq(10))+
  xlab(TeX("\\textit{j}"))+
  ylab(TeX("$\\tau_j$"))+
  ggtitle(TeX("$S_{\\lambda}$ with  $\\lambda = 0.001$"))+
  theme_bw()+
  theme(panel.grid = element_blank())

eigen1000 <- eigenvalues %>%
  ggplot()+
  geom_point(aes(x=x, y=e1000))+
  scale_x_continuous(breaks=seq(10))+
  xlab(TeX("\\textit{j}"))+
  ylab("")+
  ggtitle(TeX("$S_{\\lambda}$ with  $\\lambda = 0.01$"))+
  theme_bw()+
  theme(panel.grid = element_blank())

eigenP <- eigenvalues %>%
  ggplot()+
  geom_point(aes(x=x, y=eP))+
  scale_x_continuous(breaks=seq(10))+
  xlab(TeX("\\textit{j}"))+
  ylab("")+
  ggtitle("Projection matrix")+
  theme_bw()+
  theme(panel.grid = element_blank())

e_val_spline <- grid.arrange(eigen1, eigen1000, eigenP, ncol = 3)
ggsave(e_val_spline, file = '../../06_Plots/Splines_Theory/spline_eigenvalues.pdf', width = 7, height=3)

# Eigenvector plot
e_vectors <- data.frame(x=x, evec_2 = evec1[,2], evec_4 = evec1[,4], evec_6 = evec1[,6], evec_8 = evec1[,8])

vec_2 <- e_vectors %>%
  ggplot()+
  geom_line(aes(x=x, y=evec_2))+
  ylab(TeX("o_2"))+
  ylim(-0.2, 0.2)+
  theme_bw()+
  theme(panel.grid = element_blank())

vec_4 <- e_vectors %>%
  ggplot()+
  geom_line(aes(x=x, y=evec_4))+
  ylab(TeX("o_4"))+
  ylim(-0.2, 0.2)+
  theme_bw()+
  theme(panel.grid = element_blank())


vec_6 <- e_vectors %>%
  ggplot()+
  geom_line(aes(x=x, y=evec_6))+
  ylab(TeX("o_6"))+
  ylim(-0.2, 0.2)+
  theme_bw()+
  theme(panel.grid = element_blank())


vec_8 <- e_vectors %>%
  ggplot()+
  geom_line(aes(x=x, y=evec_8))+
  ylab(TeX("o_8"))+
  ylim(-0.2, 0.2)+
  theme_bw()+
  theme(panel.grid = element_blank())


vect <- grid.arrange(vec_2, vec_4, vec_6, vec_8, ncol = 4)
ggsave(vect, file = '../../06_Plots/Splines_Theory/spline_eigenvectors.pdf', width = 7, height = 3)

# Boosting eigenvalues
boost_30 <- eigen_boost(m=30, hat_mat=hat_mat)[1:10]
boost_50 <- eigen_boost(m=50, hat_mat=hat_mat)[1:10]
boost_110 <- eigen_boost(m=110, hat_mat=hat_mat)[1:10]
boost_500 <- eigen_boost(m=500, hat_mat=hat_mat)[1:10]

boost_eigen <- data.frame(x=seq(10), boost_30=boost_30, boost_50 = boost_50, boost_110 = boost_110, boost_500 = boost_500)

b_eigen30 <- boost_eigen %>%
  ggplot()+
  geom_point(aes(x=x, y= boost_30))+
  scale_x_continuous(breaks=seq(10))+
  ggtitle("m = 30")+
  xlab(TeX("\\textit{j}"))+
  ylab(TeX("d_{j,m}"))+
  theme_bw()+
  theme(panel.grid = element_blank())


b_eigen50 <- boost_eigen %>%
  ggplot()+
  geom_point(aes(x=x, y= boost_50))+
  scale_x_continuous(breaks=seq(10))+
  ggtitle("m = 50")+
  xlab(TeX("\\textit{j}"))+
  ylab(TeX("d_{j,m}"))+
  theme_bw()+
  theme(panel.grid = element_blank())


b_eigen110 <- boost_eigen %>%
  ggplot()+
  geom_point(aes(x=x, y= boost_110))+
  scale_x_continuous(breaks=seq(10))+
  ggtitle("m = 110")+
  xlab(TeX("\\textit{j}"))+
  ylab(TeX("d_{j,m}"))+
  theme_bw()+
  theme(panel.grid = element_blank())


b_eigen500 <- boost_eigen %>%
  ggplot()+
  geom_point(aes(x=x, y= boost_500))+
  scale_x_continuous(breaks=seq(10))+
  ggtitle("m = 500")+
  xlab(TeX("\\textit{j}"))+
  xlim(0.25, 1.00)
  ylab(TeX("d_{j,m}"))+
  theme_bw()+
  theme(panel.grid = element_blank())


b_eigen <- grid.arrange(b_eigen30, b_eigen50, b_eigen110, b_eigen500, nrow = 2, ncol = 2)
ggsave(b_eigen, file = '../../06_Plots/Boosting_Theory/eigenvalues_boosting.pdf', width = 6, height = 4)