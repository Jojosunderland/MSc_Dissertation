########################################################################
## Sensitivity Analysis ##

# After nimble MCMC
library(popbio)
library(tidyverse)

# MATRICES MADE IN MPM SCRIPT IN FIGURES

## SENSITIVITY ANALYSIS ##

# 1 unit change - 100% improvement
sens1 <- sensitivity(matrix_year1, zero = TRUE)
sens2 <- sensitivity(matrix_year2, zero = TRUE)
sens3 <- sensitivity(matrix_year3, zero = TRUE)
sens4 <- sensitivity(matrix_year4, zero = TRUE)
sens5 <- sensitivity(matrix_year5, zero = TRUE)
sens6 <- sensitivity(matrix_year6, zero = TRUE)
sens7 <- sensitivity(matrix_year7, zero = TRUE)
sens8 <- sensitivity(matrix_year8, zero = TRUE)

sens_mean <- sensitivity(mean_matrix, zero = TRUE)
sens_meanL <- sensitivity(mean_matrix, zero = TRUE)
sens_meanU <- sensitivity(mean_matrix, zero = TRUE)

# rescaling - 0.05% increase/0.05 more nestlings
sens1 <- sens1 * 0.05
sens2 <- sens2 * 0.05
sens3 <- sens3 * 0.05
sens4 <- sens4 * 0.05
sens5 <- sens5 * 0.05
sens6 <- sens6 * 0.05
sens7 <- sens7 * 0.05
sens8 <- sens8 * 0.05
sens_mean <- sens_mean * 0.05
sens_meanL <- sens_meanL * 0.05
sens_meanU <- sens_meanU * 0.05

## PLOT RESULTS ##


# All years 2018-2025
quartz()
par(mfrow=c(2,2))
image2(sens1, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2018", line = 2.5)
image2(sens2, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2019", line = 2.5)
image2(sens3, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2020", line = 2.5)
image2(sens4, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2021", line = 2.5)
quartz()
par(mfrow=c(2,2))
image2(sens5, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2022", line = 2.5)
image2(sens6, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2023", line = 2.5)
image2(sens7, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2024", line = 2.5)
image2(sens8, mar = c(2, 6, 5, 2), box.offset = 0.1)
title("2025", line = 2.5)

quartz()
par(mfrow=c(1,1))
image2(sens1, mar = c(2, 6, 8, 2), box.offset = 0.1)
title("2018", line = 2.5)
image2(sens8, mar = c(2, 6, 8, 2), box.offset = 0.1)
title("2025", line = 2.5)
image2(sens_mean, mar = c(2, 6, 8, 2), box.offset = 0.1)
title("Mean", line = 2.5)

# MEAN CIs
quartz()
par(mfrow=c(1,1))
image2(sens_meanL, mar = c(2, 6, 8, 2), box.offset = 0.1)
title("2.5%", line = 2.5)
image2(sens_mean, mar = c(2, 6, 8, 2), box.offset = 0.1)
title("Mean", line = 2.5)
image2(sens_meanU, mar = c(2, 6, 8, 2), box.offset = 0.1)
title("97.5%", line = 2.5)

