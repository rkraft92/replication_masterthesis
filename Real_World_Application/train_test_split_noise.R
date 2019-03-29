rm(list = ls())

library(caret)
library(dplyr)
library(magrittr)

set.seed(123)


#### Data Import ------
data("bodyfat", package = "TH.data")
# glimpse(bodyfat)
bodyfat <- bodyfat[,c(2,1,3:10)]

### Generating irrelevant predictor variables ------
bodyfat$x1 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x2 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x3 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x4<- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x5 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x6 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x7 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x8 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x9 <- runif(dim(bodyfat)[1], 0, 1)
bodyfat$x10 <- runif(dim(bodyfat)[1], 0, 1)

bodyfat$z1 <- sample(bodyfat$age)
bodyfat$z2 <- sample(bodyfat$waistcirc)
bodyfat$z3 <- sample(bodyfat$hipcirc)
bodyfat$z4 <- sample(bodyfat$elbowbreadth)
bodyfat$z5 <- sample(bodyfat$kneebreadth)
bodyfat$z6 <- sample(bodyfat$anthro3a)
bodyfat$z7 <- sample(bodyfat$anthro3b)
bodyfat$z8 <- sample(bodyfat$anthro3c)
bodyfat$z9 <- sample(bodyfat$anthro4)

### Container & Simulation Setup ------
n_sim <- 50

train_splits <- list()
test_splits <- list()

train_splits_noise <- list()
test_splits_noise <- list()

### Test / Train split -----------
for (mc_iter in seq(n_sim)){
  
  idx <- createDataPartition(bodyfat$DEXfat, times = 1, p = 0.7, list = F)
  
  # datasets with noise
  bodyfatTrain <- bodyfat[idx,]
  bodyfatTest <- bodyfat[-idx,]
  
  train_splits_noise[[mc_iter]] <-  bodyfatTrain
  test_splits_noise[[mc_iter]] <-  bodyfatTest
  
  # datasets without noise
  bodyfatTrain %<>% select(c(1:10))
  bodyfatTest %<>% select(c(1:10))
  
  train_splits[[mc_iter]] <-  bodyfatTrain
  test_splits[[mc_iter]] <-  bodyfatTest
}

saveRDS(train_splits_noise, file = "../99_Stored_Data/garcia_data_splits_train_noise.rds")
saveRDS(test_splits_noise, file = "../99_Stored_Data/garcia_data_splits_test_noise.rds")

saveRDS(train_splits, file = "../99_Stored_Data/garcia_data_splits_train.rds")
saveRDS(test_splits, file = "../99_Stored_Data/garcia_data_splits_test.rds")