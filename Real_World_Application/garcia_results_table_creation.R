library(xtable)

mse_noise <- readRDS(file = "../99_Stored_Data/garcia_benchmarks_noise.rds")
mse <- readRDS(file = "../99_Stored_Data/garcia_benchmark_results.rds")

results <- matrix(NA, nrow = 8, ncol = 2)

results[1:8, 1] <- apply(mse, 2, FUN = mean)
results[1:8, 2] <- apply(mse_noise, 2, FUN = mean)

results <- as.data.frame(results)
colnames(results) <- c("(Spec 1)", "(Spec 2 noise)")
Method <- c('OLS', 'L2Boosting with componentwise splines',
            'L2Boosting with linear least squares','StepAIC', 
            'LASSO', 'MARS', 'additive model (backfitted)', 
            "L2Boosting with componentwise stumps")
output <- cbind(Method, results)

print(xtable(output, digits = 4, type = "latex"), include.rownames = FALSE, file = "../../05_Text/Real_World_Application/garcia_results.tex")
