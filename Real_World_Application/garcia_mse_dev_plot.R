rm(list = ls())

'
Plot resulting data from schmid_hothorn.R mse decomposition
'

n_boost <- 300
boost_variance_bias_mse_glm <- readRDS("../99_Stored_Data/garcia_mse_development_glm.rds")
boost_variance_bias_mse_gam <- readRDS("../99_Stored_Data/garcia_mse_development_glm.rds")

pdf("../../06_Plots/Real_World_Application/garcia_bias_variance_glm.pdf", width = 6, height=4)
plot(seq(n_boost), apply(boost_variance_bias_mse_glm, 2, mean), 
     type = "l", 
     ylim = c(10,15),
     xlab = "Boosting iterations",
     ylab = "MSE",
     main = "Real World Application - MSE Development")
abline(h = 11.6955, lty = 2)
dev.off()


pdf("../../06_Plots/Real_World_Application/garcia_bias_variance_gam.pdf", width = 6, height=4)
plot(seq(n_boost), apply(boost_variance_bias_mse_gam, 2, mean), 
     type = "l", 
     ylim = c(10,15),
     xlab = "Boosting iterations",
     ylab = "MSE",
     main = "Real World Application - MSE Development")
abline(h = 11.7536, lty = 2)
dev.off()


##### GGPLOT2 IMPLEMENTATION
library(ggplot2)
library(dplyr)
library(gridExtra)

df <- data.frame(x = seq(n_boost), MSE = apply(boost_variance_bias_mse_gam, 2, mean))
df_plot <- df %>% 
  ggplot()+
  geom_line(aes(x=x, y= MSE, linetype = "AIC-stopped"))+
  geom_line(aes(x=x, y=11.7536, linetype = "MSE-stopped"))+
  xlab("Boosting iterations")+
  ylab("MSE")+
  ggtitle("Real World Application - MSE Development")+
  ylim(10,15)+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom")+
  scale_linetype_manual("", values = c(1,2), labels = c("MSE", "AIC-stopped"))

ggsave(df_plot, file = "../../06_Plots/Real_World_Application/garcia_bias_variance_gam.pdf", width = 6, height = 4)
