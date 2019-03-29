rm(list = ls())

boost_object <- readRDS("../99_Stored_Data/garcia_boost_objects.rds")

# pdf("../../06_Plots/Real_World_Application/garcia_partial_effects_gam.pdf")
# par(mfrow = c(2,3))
# plot(boost_object[[1]])
# dev.off()

pdf("../../06_Plots/Real_World_Application/garcia_linear_effects_glm.pdf", width = 6, height=5)
par(mar = c(5,5,1,6))
plot(boost_object[[2]], ylim = c(0, 4), main = "")
dev.off()


boost_object_noise <- readRDS("../99_Stored_Data/garcia_boost_objects_noise.rds")

pdf("../../06_Plots/Real_World_Application/garcia_partial_effects_gam_noise.pdf", width = 6, height=8.5)
par(mfrow = c(3,3))
plot(boost_object_noise[[1]],  ylim = c(-6,6), main = "")
dev.off()

pdf("../../06_Plots/Real_World_Application/garcia_linear_effects_glm_noise.pdf", width = 6, height=5)
par(mar = c(5,5,1,6))
plot(boost_object_noise[[2]],  ylim = c(-2,4), main = "")
dev.off()