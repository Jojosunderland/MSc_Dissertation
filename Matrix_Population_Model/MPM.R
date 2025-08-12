########################################################################
## MPM Construction ##

# After nimble MCMC
library(popbio)
library(tidyverse)


## Create Matrices ##

matrix_samples <- Matrix_model

# YEAR 1 (2018)
mean_vals1 <- matrix_samples[grepl("matrix_model\\[.*, .*, 1\\]", rownames(matrix_samples)), "mean"]
matrix_year1 <- matrix(mean_vals1, nrow = 4, ncol = 4)
rownames(matrix_year1) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year1) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 2 (2019)
mean_vals2 <- matrix_samples[grepl("matrix_model\\[.*, .*, 2\\]", rownames(matrix_samples)), "mean"]
matrix_year2 <- matrix(mean_vals2, nrow = 4, ncol = 4)
rownames(matrix_year2) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year2) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 3 (2020)
mean_vals3 <- matrix_samples[grepl("matrix_model\\[.*, .*, 3\\]", rownames(matrix_samples)), "mean"]
matrix_year3 <- matrix(mean_vals3, nrow = 4, ncol = 4)
rownames(matrix_year3) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year3) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 4 (2021)
mean_vals4 <- matrix_samples[grepl("matrix_model\\[.*, .*, 4\\]", rownames(matrix_samples)), "mean"]
matrix_year4 <- matrix(mean_vals4, nrow = 4, ncol = 4)
rownames(matrix_year4) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year4) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 5 (2022)
mean_vals5 <- matrix_samples[grepl("matrix_model\\[.*, .*, 5\\]", rownames(matrix_samples)), "mean"]
matrix_year5 <- matrix(mean_vals5, nrow = 4, ncol = 4)
rownames(matrix_year5) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year5) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 6 (2023)
mean_vals6 <- matrix_samples[grepl("matrix_model\\[.*, .*, 6\\]", rownames(matrix_samples)), "mean"]
matrix_year6 <- matrix(mean_vals6, nrow = 4, ncol = 4)
rownames(matrix_year6) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year6) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 7 (2024)
mean_vals7 <- matrix_samples[grepl("matrix_model\\[.*, .*, 7\\]", rownames(matrix_samples)), "mean"]
matrix_year7 <- matrix(mean_vals7, nrow = 4, ncol = 4)
rownames(matrix_year7) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year7) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 8 (2025)
mean_vals8 <- matrix_samples[grepl("matrix_model\\[.*, .*, 8\\]", rownames(matrix_samples)), "mean"]
matrix_year8 <- matrix(mean_vals8, nrow = 4, ncol = 4)
rownames(matrix_year8) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year8) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# Print yearly matrices

print(matrix_year1)
print(matrix_year2)
print(matrix_year3)
print(matrix_year4)
print(matrix_year5)
print(matrix_year6)
print(matrix_year7)
print(matrix_year8)


# AVERAGE MATRIX

n_stages <- 4
n_years <-  8 

mean_matrix <- matrix(NA, nrow = n_stages, ncol = n_stages)

for(i in 1:n_stages){
  for(j in 1:n_stages){
    # Pattern to match all years for element [i, j, y]
    pattern <- paste0("matrix_model\\[", i, "\\s*,\\s*", j, "\\s*,")
    idx <- grep(pattern, rownames(matrix_samples))
    vals <- matrix_samples[idx, "mean"]
    mean_matrix[i, j] <- mean(vals)
  }
}

rownames(mean_matrix) <-c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(mean_matrix) <- c("N (t)", "J (t)", "SA (t)", "A (t)")
print(mean_matrix)



# save row and column names
rownames(mean_matrix) <- c("Nestling (t+1)", "Juvenile (t+1)", "Subadult (t+1)", "Adult (t+1)")
colnames(mean_matrix) <- c("Nestling (t)", "Juvenile (t)", "Subadult (t)", "Adult (t)")

rownames(matrix_year1) <- c("Nestling (t+1)", "Juvenile (t+1)", "Subadult (t+1)", "Adult (t+1)")
colnames(matrix_year1) <- c("Nestling (t)", "Juvenile (t)", "Subadult (t)", "Adult (t)")

rownames(matrix_year8) <- c("Nestling (t+1)", "Juvenile (t+1)", "Subadult (t+1)", "Adult (t+1)")
colnames(matrix_year8) <- c("Nestling (t)", "Juvenile (t)", "Subadult (t)", "Adult (t)")


## REPEAT FOR CI's ##

# YEAR 1 (2018)
mean_vals1 <- matrix_samples[grepl("matrix_model\\[.*, .*, 1\\]", rownames(matrix_samples)), "2.5%"]
matrix_year1 <- matrix(mean_vals1, nrow = 4, ncol = 4)
rownames(matrix_year1) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year1) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 2 (2019)
mean_vals2 <- matrix_samples[grepl("matrix_model\\[.*, .*, 2\\]", rownames(matrix_samples)), "2.5%"]
matrix_year2 <- matrix(mean_vals2, nrow = 4, ncol = 4)
rownames(matrix_year2) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year2) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 3 (2020)
mean_vals3 <- matrix_samples[grepl("matrix_model\\[.*, .*, 3\\]", rownames(matrix_samples)), "2.5%"]
matrix_year3 <- matrix(mean_vals3, nrow = 4, ncol = 4)
rownames(matrix_year3) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year3) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 4 (2021)
mean_vals4 <- matrix_samples[grepl("matrix_model\\[.*, .*, 4\\]", rownames(matrix_samples)), "2.5%"]
matrix_year4 <- matrix(mean_vals4, nrow = 4, ncol = 4)
rownames(matrix_year4) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year4) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 5 (2022)
mean_vals5 <- matrix_samples[grepl("matrix_model\\[.*, .*, 5\\]", rownames(matrix_samples)), "2.5%"]
matrix_year5 <- matrix(mean_vals5, nrow = 4, ncol = 4)
rownames(matrix_year5) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year5) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 6 (2023)
mean_vals6 <- matrix_samples[grepl("matrix_model\\[.*, .*, 6\\]", rownames(matrix_samples)), "2.5%"]
matrix_year6 <- matrix(mean_vals6, nrow = 4, ncol = 4)
rownames(matrix_year6) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year6) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 7 (2024)
mean_vals7 <- matrix_samples[grepl("matrix_model\\[.*, .*, 7\\]", rownames(matrix_samples)), "2.5%"]
matrix_year7 <- matrix(mean_vals7, nrow = 4, ncol = 4)
rownames(matrix_year7) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year7) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 8 (2025)
mean_vals8 <- matrix_samples[grepl("matrix_model\\[.*, .*, 8\\]", rownames(matrix_samples)), "2.5%"]
matrix_year8 <- matrix(mean_vals8, nrow = 4, ncol = 4)
rownames(matrix_year8) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year8) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# Print yearly matrices

print(matrix_year1)
print(matrix_year2)
print(matrix_year3)
print(matrix_year4)
print(matrix_year5)
print(matrix_year6)
print(matrix_year7)
print(matrix_year8)


# AVERAGE MATRIX

n_stages <- 4
n_years <- 8 

mean_matrix <- matrix(NA, nrow = n_stages, ncol = n_stages)

for(i in 1:n_stages){
  for(j in 1:n_stages){
    # Pattern to match all years for element [i, j, y]
    pattern <- paste0("matrix_model\\[", i, "\\s*,\\s*", j, "\\s*,")
    idx <- grep(pattern, rownames(matrix_samples))
    vals <- matrix_samples[idx, "2.5%"]
    mean_matrix[i, j] <- mean(vals)
  }
}

rownames(mean_matrix) <-c("Nestling (t+1)", "Juvenile (t+1)", "Subadult (t+1)", "Adult (t+1)")
colnames(mean_matrix) <- c("Nestling (t)", "Juvenile (t)", "Subadult (t)", "Adult (t)")


rownames(mean_matrix) <-c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(mean_matrix) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

print(mean_matrix)


# YEAR 1 (2018)
mean_vals1 <- matrix_samples[grepl("matrix_model\\[.*, .*, 1\\]", rownames(matrix_samples)), "97.5%"]
matrix_year1 <- matrix(mean_vals1, nrow = 4, ncol = 4)
rownames(matrix_year1) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year1) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 2 (2019)
mean_vals2 <- matrix_samples[grepl("matrix_model\\[.*, .*, 2\\]", rownames(matrix_samples)), "97.5%"]
matrix_year2 <- matrix(mean_vals2, nrow = 4, ncol = 4)
rownames(matrix_year2) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year2) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 3 (2020)
mean_vals3 <- matrix_samples[grepl("matrix_model\\[.*, .*, 3\\]", rownames(matrix_samples)), "97.5%"]
matrix_year3 <- matrix(mean_vals3, nrow = 4, ncol = 4)
rownames(matrix_year3) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year3) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 4 (2021)
mean_vals4 <- matrix_samples[grepl("matrix_model\\[.*, .*, 4\\]", rownames(matrix_samples)), "97.5%"]
matrix_year4 <- matrix(mean_vals4, nrow = 4, ncol = 4)
rownames(matrix_year4) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year4) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 5 (2022)
mean_vals5 <- matrix_samples[grepl("matrix_model\\[.*, .*, 5\\]", rownames(matrix_samples)), "97.5%"]
matrix_year5 <- matrix(mean_vals5, nrow = 4, ncol = 4)
rownames(matrix_year5) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year5) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 6 (2023)
mean_vals6 <- matrix_samples[grepl("matrix_model\\[.*, .*, 6\\]", rownames(matrix_samples)), "97.5%"]
matrix_year6 <- matrix(mean_vals6, nrow = 4, ncol = 4)
rownames(matrix_year6) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year6) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 7 (2024)
mean_vals7 <- matrix_samples[grepl("matrix_model\\[.*, .*, 7\\]", rownames(matrix_samples)), "97.5%"]
matrix_year7 <- matrix(mean_vals7, nrow = 4, ncol = 4)
rownames(matrix_year7) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year7) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# YEAR 8 (2025)
mean_vals8 <- matrix_samples[grepl("matrix_model\\[.*, .*, 8\\]", rownames(matrix_samples)), "97.5%"]
matrix_year8 <- matrix(mean_vals8, nrow = 4, ncol = 4)
rownames(matrix_year8) <- c("N (t+1)", "J (t+1)", "SA (t+1)", "A (t+1)")
colnames(matrix_year8) <- c("N (t)", "J (t)", "SA (t)", "A (t)")

# Print yearly matrices

print(matrix_year1)
print(matrix_year2)
print(matrix_year3)
print(matrix_year4)
print(matrix_year5)
print(matrix_year6)
print(matrix_year7)
print(matrix_year8)

# AVERAGE MATRIX

n_stages <- 4
n_years <- 8

mean_matrix <- matrix(NA, nrow = n_stages, ncol = n_stages)

for(i in 1:n_stages){
  for(j in 1:n_stages){
    # Pattern to match all years for element [i, j, y]
    pattern <- paste0("matrix_model\\[", i, "\\s*,\\s*", j, "\\s*,")
    idx <- grep(pattern, rownames(matrix_samples))
    vals <- matrix_samples[idx, "97.5%"]
    mean_matrix[i, j] <- mean(vals)
  }
}

rownames(mean_matrix) <-c("Nestling (t+1)", "Juvenile (t+1)", "Subadult (t+1)", "Adult (t+1)")
colnames(mean_matrix) <- c("Nestling (t)", "Juvenile (t)", "Subadult (t)", "Adult (t)")

print(mean_matrix)
