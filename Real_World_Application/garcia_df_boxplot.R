rm(list = ls())

'
Plot resulting data from schmid_hothorn.R as boxplots
'

mse_df <- readRDS("../99_Stored_Data/garcia_degrees_of_freedom.rds")

pdf("../../06_Plots/Real_World_Application/garcia_boxplot.pdf", width = 6, height=4)
boxplot(mse_df, use.cols = TRUE, xlab = "Degrees of freedom", ylab = "MSE", xaxt = "n")
axis(1, at = 1:8, labels = c(2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6))
dev.off()



##### GGPLOT2 Implementation ------
library(ggplot2)
mse_df_dataframe <- data.frame(mse_df)
box <- ggplot(stack(mse_df_dataframe), aes(x = ind, y = values))+
  geom_boxplot()+
  scale_x_discrete(labels=c("X1" = "2.5", "X2" = "3", "X3" = "3.5", "X4" = "4", "X5" = "4.5", "X6" = "5", "X7" = "5.5", "X8" = "6"))+
  xlab("Degrees of freedom")+
  ylab("MSE")+
  theme_bw()+
  theme(panel.grid = element_blank())

ggsave(box, file = "../../06_Plots/Real_World_Application/garcia_boxplot.pdf", width = 6, height=3)