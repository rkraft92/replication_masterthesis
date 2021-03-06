rm(list = ls())
set.seed(1234)
library('mboost')

x <- runif(150, -0.2, 0.2)
y = (0.5 - 0.9* exp(-50*x^2))*x + 0.02 *rnorm(150)
y <- y[order(x)] ## order obs by size of x
x <- x[order(x)] ## just for easier plotting

pdf('../../06_Plots/Boosting_Theory/residualplot_splines.pdf')
par(mfrow = c(4,2)) ## nrows, ncols for layout

#### m=1 ------
## model fit
plot(x, y, las = 1, main = "model fit at m = 1" ) ## observations
curve((0.5 - 0.9* exp(-50*x^2))*x, add=TRUE, from = -.2,
         to = .2, lty = 2, lwd = 2) ## true function
## now carry out one boosting iteration
gam1 <- gamboost(y ~ x, control = boost_control(mstop = 1))
lines(x , fitted(gam1), col = 2, lwd = 2) ## plot fitted values

## residual plot
plot(x, y - fitted(gam1[1]) , ylab = "residuals", main = "residuals at m = 1",
           ylim = c(-.1, .1), las = 1) ## residuals
lines(smooth.spline(x, y - fitted(gam1)),
          col = 4, lwd = 2) ## show remaining structure

#### m=30 ------
## model fit
 plot(x, y, las = 1, main = "model fit at m = 30" )
 curve((0.5 - 0.9* exp(-50*x^2))*x, add=TRUE, from = -.2, to = 0.2,
           lty =2, lwd = 2)
 lines(x , fitted(gam1[30]), col = 2, lwd = 2)

## residual plot
 plot(x, y - fitted(gam1[30]) , ylab = "residuals", main = "residuals at m = 30",
            las = 1, ylim = c(-.1, .1))
 lines(smooth.spline(x, y - fitted(gam1[30])), col = 4, lwd = 2)

### m=50 ---------
 ## model fit
 plot(x, y, las = 1, main = "model fit at m = 50" )
 curve((0.5 - 0.9* exp(-50*x^2))*x, add=TRUE, from = -.2, to = 0.2,
       lty =2, lwd = 2)
 lines(x , fitted(gam1[50]), col = 2, lwd = 2)
 
 ## residual plot
 plot(x, y - fitted(gam1[50]) , ylab = "residuals", main = "residuals at m = 50",
      las = 1, ylim = c(-.1, .1))
 lines(smooth.spline(x, y - fitted(gam1[50])), col = 4, lwd = 2)

 ### m=110 ---------
 ## model fit
 plot(x, y, las = 1, main = "model fit at m = 110" )
 curve((0.5 - 0.9* exp(-50*x^2))*x, add=TRUE, from = -.2, to = 0.2,
       lty =2, lwd = 2)
 lines(x , fitted(gam1[110]), col = 2, lwd = 2)
 
 ## residual plot
 plot(x, y - fitted(gam1[110]) , ylab = "residuals", main = "residuals at m = 110",
      las = 1, ylim = c(-.1, .1))
 lines(smooth.spline(x, y - fitted(gam1[110])), col = 4, lwd = 2)
 
 dev.off()


pdf('../../06_Plots/Boosting_Theory/residualplot_splines_overfitting.pdf', width = 6, height=4)
par(mfrow = c(1,2))
#### overfitting ----------
 ## model fit
 plot(x, y, las = 1, main = "model fit at m = 1000" )
 curve((0.5 - 0.9* exp(-50*x^2))*x, add=TRUE, from = -.2, to = 0.2,
       lty =2, lwd = 2)
 lines(x , fitted(gam1[1000]), col = 2, lwd = 2)
 
 ## model fit
 plot(x, y, las = 1, main = "model fit at m = 50000" )
 curve((0.5 - 0.9* exp(-50*x^2))*x, add=TRUE, from = -.2, to = 0.2,
       lty =2, lwd = 2)
 lines(x , fitted(gam1[50000]), col = 2, lwd = 2)
dev.off() 


##### GGPLOT2 IMPLEMENTATION -----
library(ggplot2)
library(dplyr)
library(latex2exp)
library(gridExtra)
#### m=1 ------
## model fit
initial <- data.frame(x = x, y = y, true = (0.5 - 0.9* exp(-50*x^2))*x, 
                      g1 = fitted(gam1[1]), 
                      resid1 = y - fitted(gam1[1]),
                      resid_spline = fitted(smooth.spline(x, y - fitted(gam1[1]))))


m1 <- initial %>%
  ggplot()+
  geom_point(aes(x=x, y=y), shape = 21)+
  geom_line(aes(x=x, y=true))+
  geom_line(aes(x=x, y=g1))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  xlab("x")+
  ylab("y")+
  ggtitle(TeX("model fit at $m$ = 1"))

resid_1 <- initial %>%
  ggplot()+
  geom_point(aes(x=x, y=resid1))+
  geom_line(aes(x=x, y=resid_spline))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  xlab("x")+
  ylab("residuals")+
  ggtitle(TeX("residuals at $m = 1$"))


### m = 5 -----
initial_5 <- data.frame(x = x, y = y, true = (0.5 - 0.9* exp(-50*x^2))*x, 
                      g5 = fitted(gam1[5]), 
                      resid5 = y - fitted(gam1[5]),
                      resid_spline = fitted(smooth.spline(x, y - fitted(gam1[5]))))

m5 <- initial_5 %>%
  ggplot()+
  geom_point(aes(x=x, y=y), shape = 21)+
  geom_line(aes(x=x, y=true, linetype = "true"), size = 0.75)+
  geom_line(aes(x=x, y=g5, color = "boost"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("y")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("model fit at $m = 5$"))+
  scale_linetype_manual(name = "", 
                        values=c(2), labels=c("true"))+
  scale_color_manual(name = "", 
                     values=c("red"), labels=c("boost"))

resid_5 <- initial_5 %>%
  ggplot()+
  geom_point(aes(x=x, y=resid5), shape = 21)+
  geom_line(aes(x=x, y=resid_spline, color = "resid"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("residuals")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("residuals at $m = 5$"))+
  scale_color_manual(name = "", 
                     values=c("blue"), labels=c("resid"))


##### m = 50 -------
initial_30 <- data.frame(x = x, y = y, true = (0.5 - 0.9* exp(-50*x^2))*x, 
                         g30 = fitted(gam1[30]), 
                         resid30 = y - fitted(gam1[30]),
                         resid_spline = fitted(smooth.spline(x, y - fitted(gam1[30]))))


m30 <- initial_30 %>%
  ggplot()+
  geom_point(aes(x=x, y=y), shape = 21)+
  geom_line(aes(x=x, y=true, linetype = "true"), size = 0.75)+
  geom_line(aes(x=x, y=g30, color = "boost"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("y")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("model fit at $m = 30$"))+
  scale_linetype_manual(name = "", 
                        values=c(2), labels=c("true"))+
  scale_color_manual(name = "", 
                     values=c("red"), labels=c("boost"))

resid_30 <- initial_30 %>%
  ggplot()+
  geom_point(aes(x=x, y=resid30), shape = 21)+
  geom_line(aes(x=x, y=resid_spline, color = "resid"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("residuals")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("residuals at $m = 30$"))+
  scale_color_manual(name = "", 
                     values=c("blue"), labels=c("resid"))


###### m = 110 --------
initial_110 <- data.frame(x = x, y = y, true = (0.5 - 0.9* exp(-50*x^2))*x, 
                         g110 = fitted(gam1[110]), 
                         resid110 = y - fitted(gam1[110]),
                         resid_spline = fitted(smooth.spline(x, y - fitted(gam1[110]))))

m110 <- initial_110 %>%
  ggplot()+
  geom_point(aes(x=x, y=y), shape = 21)+
  geom_line(aes(x=x, y=true, linetype = "true"), size = 0.75)+
  geom_line(aes(x=x, y=g110, color = "boost"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("y")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("model fit at $m = 110$"))+
  scale_linetype_manual(name = "", 
                        values=c(2), labels=c("true"))+
  scale_color_manual(name = "", 
                        values=c("red"), labels=c("boost"))

resid_110 <- initial_110 %>%
  ggplot()+
  geom_point(aes(x=x, y=resid110), shape = 21)+
  geom_line(aes(x=x, y=resid_spline, color = "resid"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("residuals")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("residuals at $m = 110$"))+
  scale_color_manual(name = "", 
                     values=c("blue"), labels=c("resid"))


comb <- grid.arrange(m5, resid_5, m30, resid_30, m110, resid_110, ncol = 2)
ggsave(comb, file = '../../06_Plots/Boosting_Theory/residualplot_splines.pdf', width =6, height = 8.5)

##### Overfitting ------
initial_1000 <- data.frame(x = x, y = y, true = (0.5 - 0.9* exp(-50*x^2))*x, 
                          g1000 = fitted(gam1[1000]), 
                          resid1000 = y - fitted(gam1[1000]),
                          resid_spline = fitted(smooth.spline(x, y - fitted(gam1[1000]))))

m1000 <- initial_1000 %>%
  ggplot()+
  geom_point(aes(x=x, y=y), shape = 21)+
  geom_line(aes(x=x, y=true, linetype = "true"), size = 0.75)+
  geom_line(aes(x=x, y=g1000, color = "boost"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("y")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("model fit at $m = 1000$"))+
  scale_linetype_manual(name = "", 
                        values=c(2), labels=c("true"))+
  scale_color_manual(name = "", 
                     values=c("red"), labels=c("boost"))


initial_50000 <- data.frame(x = x, y = y, true = (0.5 - 0.9* exp(-50*x^2))*x, 
                           g50000 = fitted(gam1[50000]), 
                           resid50000 = y - fitted(gam1[50000]),
                           resid_spline = fitted(smooth.spline(x, y - fitted(gam1[50000]))))

m50000 <- initial_50000 %>%
  ggplot()+
  geom_point(aes(x=x, y=y), shape = 21)+
  geom_line(aes(x=x, y=true, linetype = "true"), size = 0.75)+
  geom_line(aes(x=x, y=g50000, color = "boost"))+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position="none")+
  xlab("x")+
  ylab("y")+
  ylim(-0.1, 0.1)+
  ggtitle(TeX("model fit at $m = 50000$"))+
  scale_linetype_manual(name = "", 
                        values=c(2), labels=c("true"))+
  scale_color_manual(name = "", 
                     values=c("red"), labels=c("boost"))

comb1 <- grid.arrange(m1000, m50000, ncol = 2)
ggsave(comb1, file = '../../06_Plots/Boosting_Theory/residualplot_splines_overfitting.pdf', width = 6, height = 3)

