rm(list = ls())

'
Plot resulting data from schmid_hothorn.R mse decomposition
'

n_boost <- 300
boost_variance_bias_mse <- readRDS("../99_Stored_Data/schmid_hothorn_boost_iter_bias_variance.rds")

pdf("../../06_Plots/Main_Simulation/schmid_hothorn_bias_variance.pdf", width = 6, height=4)
plot(seq(n_boost), boost_variance_bias_mse[3,], 
     type = "l", 
     ylim = c(0,20),
     xlab = "Boosting iterations",
     ylab = "",
     main = "MSE Decomposition")
lines(seq(n_boost), boost_variance_bias_mse[1,], lty = 2)
lines(seq(n_boost), boost_variance_bias_mse[2,], lty = 3)
dev.off()


#### GGPLOT2 IMPLEMENTATION --------
library(ggplot2)
library(dplyr)
library(gridExtra)

df <- data.frame(x = seq(n_boost), MSE = boost_variance_bias_mse[3,1:300], variance = boost_variance_bias_mse[1,1:300], bias = boost_variance_bias_mse[2,1:300])
df_plot <- df %>% 
  ggplot()+
  geom_line(aes(x=x, y= MSE, linetype = "MSE"))+
  geom_line(aes(x=x, y= variance, linetype = "Variance"))+
  geom_line(aes(x=x, y= bias, linetype = "Squared Bias"))+
  xlab("Boosting iterations")+
  ylim(0,20)+
  ggtitle("MSE Decomposition")+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")+
  scale_linetype_manual(name = "", 
                        values=c(1,3,2), labels=c("MSE","Squared Bias", "Variance"))
ggsave(df_plot, file = '../../06_Plots/Main_Simulation/schmid_hothorn_bias_variance.pdf', width = 6, height = 4)
