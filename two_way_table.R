library(mice)

imputed_data <- readRDS("../STAT8101/datasets/imputed_data.RDS")
complete_data <- complete(imputed_data, 1)

raw_data <- read.csv("../STAT8101/datasets/so_survey_results_public.csv") 

clean_and_filter_data <- function(raw_data, complete_data, column_name) {
  # Extract unique values and clean them
  unique_values <- unique(data[[column_name]])
  unique_values <- as.list(na.omit(unique_values))
  
  unique_values <- gsub("’", "", unique_values)
  unique_values <- gsub("'", "", unique_values)
  unique_values <- gsub("[[:punct:]]", "_", unique_values)
  unique_values <- gsub(" ", "_", unique_values)
  unique_values <- gsub("Professional_", "Prof_", unique_values) 
  
  add_x_prefix <- function(labels) {
    labels <- ifelse(grepl("^[0-9]", labels), paste0("X", labels), labels)
    return(labels)
  }
  
  unique_values <- add_x_prefix(unique_values)
  
  # Filter data based on cleaned names
  filtered_data <- complete_data[, unique_values, drop = FALSE]
  
  get_true_columns <- function(row) {
    # Get the names of columns where the value is 1
    names <- names(row)[row == 1]
    # Combine the names into a single string
    paste(names, collapse = ", ")
  }
  
  # Apply to each row
  result_column <- apply(filtered_data, 1, get_true_columns)
  
  return(data.frame(Result = result_column))
}

# amalgamate the data
ed_result <- clean_and_filter_data(raw_data, complete_data, "EdLevel")
org_result <- clean_and_filter_data(raw_data, complete_data, "OrgSize")

# conduct chi-squared test
education_levels <- ed_result$Result
organization_sizes <- org_result$Result

table_data <- data.frame(Education = education_levels, OrganizationSize = organization_sizes)
contingency_table <- table(table_data$Education, table_data$OrganizationSize)

chi_test <- chisq.test(contingency_table)

print(chi_test)

# Check chi-squared assumptions 
expected_frequencies <- chi_test$expected

# Find cells with expected frequencies less than 5
low_freq_indices <- which(expected_frequencies < 5, arr.ind = TRUE)

if(length(low_freq_indices) > 0) {
  cat("Cells with expected frequencies less than 5:\n")
  for (idx in 1:nrow(low_freq_indices)) {
    row_idx <- low_freq_indices[idx, 1]
    col_idx <- low_freq_indices[idx, 2]
    cat(sprintf("Row %d, Column %d: Expected Frequency = %f\n", 
                row_idx, col_idx, expected_frequencies[row_idx, col_idx]))
  }
} else {
  cat("All expected frequencies are 5 or greater.\n")
}

# Fisher test is more appropriate due to low frequency data
# Perform Fisher's Exact Test using simulation for the p-value
fisher_test_simulated <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)

print(fisher_test_simulated)
