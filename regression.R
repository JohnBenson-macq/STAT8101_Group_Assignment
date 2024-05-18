library(MASS)
library(mice)
library(ggplot2)
library(dplyr)
library(broom)  
library(boot)

imputed_data <- readRDS("../STAT8101/datasets/imputed_data.RDS")
data <- read.csv("../STAT8101/datasets/so_survey_results_public.csv")

# Filter and extract the dependent variable
data <- filter(data, !is.na(ConvertedCompYearly))
dependent <- data$ConvertedCompYearly

rm(data)
gc() 

# Create a single imputed dataset using the first imputation
# I run out of memory doing pooled runs
complete_data <- complete(imputed_data, action = 1)
complete_data$ConvertedCompYearly <- dependent

rm(imputed_data)
gc() 

# Separating predictors and dependent variable
dependent <- complete_data$ConvertedCompYearly
predictors <- complete_data[, setdiff(names(complete_data), "ConvertedCompYearly")]

# Perform PCA on the predictors
pca_result <- prcomp(predictors, scale. = TRUE)  # Standardize variables

# Examine the summary of PCA to decide how many components to retain
print(summary(pca_result))

# Scree plot to visualize variance explained by each principal component
screeplot(pca_result, type = "lines", main = "Scree Plot")

# We need to slice out some otherwise the regression fails
cum_var_explained <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
num_components <- max(which(cum_var_explained < 0.9))
pc_predictors <- pca_result$x[, 1:num_components]

# Create a data frame to include PC predictors and the dependent variable
pc_data <- data.frame(pc_predictors, ConvertedCompYearly = dependent)

rm(list = c("complete_data", "predictors"))
gc()

# Run Huber regression on the PC data
formula <- as.formula(paste("ConvertedCompYearly ~", paste(names(pc_data)[-which(names(pc_data) == "ConvertedCompYearly")], collapse = " + ")))
huber_model <- rlm(formula, data = pc_data, psi = psi.huber)

summary(huber_model)

# Coefficients from the Huber model
huber_coeffs <- coef(huber_model)

# Calculate the effective contributions
# Each row in loadings corresponds to an original predictor,
# and each column corresponds to a principal component.
# huber_coeffs will be multiplied accordingly to map these contributions back to the original predictors.
loadings <- pca_result$rotation[, 1:num_components]
effective_contributions <- loadings %*% diag(huber_coeffs[-1])  # Assuming first coeff is the intercept

# Sum contributions across components to get the total contribution of each predictor
total_contributions <- apply(effective_contributions, 1, sum)
predictor_contributions <- data.frame(Predictor = rownames(loadings), Contribution = total_contributions)

predictor_contributions <- predictor_contributions[order(abs(predictor_contributions$Contribution), decreasing = TRUE), ]
print(predictor_contributions)

# Function to perform Huber regression and extract coefficients
huber_coef <- function(data, indices) {
  sample_data <- data[indices, ]  # resampling with replacement
  model <- rlm(formula, data = sample_data, psi = psi.huber)
  coef(model)
}

# Bootstrapping
set.seed(123)  # for reproducibility
bootstrap_results <- boot(data = pc_data, statistic = huber_coef, R = 1000)

# Calculating confidence intervals
boot_ci <- boot.ci(bootstrap_results, type = c("bca"))

print(boot_ci)
