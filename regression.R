# install.packages("MTE")
# install.packages("Matrix")

library(MASS)
library(mice)
library(ggplot2)
library(dplyr)
library(broom)  
library(boot)
library(parallel)
library(glmnet)
library(dplyr)
library(MTE)
library(nloptr)
library(numDeriv)

set.seed(123)  # For reproducibility

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

X <- as.matrix(predictors)
y <- as.vector(dependent)
y_log <- log(y)

data <- data.frame(
  Value = c(y, y_log),
  Type = c(rep("Original", length(y)), rep("Log-transformed", length(y_log)))
)

boxplot(Value ~ Type, data = data,
        main = "Developer Compensation comparison of skewness",
        ylab = "Compensation (USD)",
        col = c("lightblue", "lightgreen"),
        border = "black",
        outline = TRUE) 

# Perform cross-validation for lambda selection
cv_lasso <- cv.glmnet(X, y_log, alpha = 1)  # alpha = 1 for Lasso

# Extract the lambda values, MSE, and the standard errors
lambda <- cv_lasso$lambda
log_lambda <- log(lambda)
mse <- cv_lasso$cvm
std_error <- cv_lasso$cvsd

# Create a data frame for plotting
data <- data.frame(log_lambda, mse, std_error)

# Plot using ggplot2
plot <- ggplot(data, aes(x = log_lambda, y = mse)) +
  geom_line() +  # Plot the line connecting MSE points
  geom_point() +  # Add points for each lambda
  geom_errorbar(aes(ymin = mse - std_error, ymax = mse + std_error), width = .1) +
  labs(x = "Log(Lambda)", y = "Mean Squared Error", title = "Cross-Validation Results for LASSO Regression") +
  theme_minimal()  # Use a minimal theme for cleaner presentation

# Print the plot
print(plot)

# select best lambda 
best_lambda <- cv_lasso$lambda.min
final_model <- glmnet(X, y_log, alpha = 1, lambda = best_lambda)
coefficients_final <- coef(final_model, s = "lambda.min")

# Make predictions
predictions <- predict(final_model, newx = X, s = "lambda.min")

# Convert predictions back to the original scales
predictions_original_scale <- exp(predictions)

# Mean Squared Error
mse <- mean((exp(y_log) - predictions_original_scale)^2)
cat("Mean Squared Error: ", mse, "\n")

# Plot residuals and control for outliers
residuals <- exp(y_log) - predictions_original_scale
mean_residuals <- mean(residuals)
sd_residuals <- sd(residuals)
cutoff_residuals <- mean_residuals + 3 * sd_residuals  # Upper limit at 3 standard deviations
residuals_capped <- ifelse(residuals > cutoff_residuals, cutoff_residuals, residuals)

# Residuals
plot(residuals_capped, main = "Residuals", xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")

# Predicted vs. residuals
plot(predictions_original_scale, residuals_capped, main = "Predicted vs Residuals", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Calculate R-squared
ss_total <- sum((exp(y_log) - mean(exp(y_log)))^2)
ss_resid <- sum((exp(y_log) - predictions_original_scale)^2)
r_squared <- 1 - ss_resid/ss_total
cat("R-squared: ", r_squared, "\n")

# Extract coefficients
coef_matrix <- coef(final_model, s = "lambda.min")  
coefficients_vector <- as.vector(coef_matrix)  
coefficients_df <- data.frame(Coefficient = coefficients_vector[-1])  # Remove intercept
rownames(coefficients_df) <- colnames(X)
coefficients_df$abs_coef <- abs(coefficients_df$Coefficient)
coefficients_df_sorted <- coefficients_df[order(-coefficients_df$abs_coef), ]

# Select top 10 predictors
top_predictors <- head(coefficients_df_sorted, 10)
print(top_predictors)