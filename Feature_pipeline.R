# install.packages("naniar")
# install.packages("missMDA")
# install.packages("mice")
# install.packages("caret")

library(naniar)  
library(missMDA)
library(mice)
library(dplyr)
library(tidyr)
library(caret)

data <- read.csv("../STAT8101/datasets/so_survey_results_public.csv") 

data_subset <- data[, c('ConvertedCompYearly', 'EdLevel', 'DevType', 'YearsCode', 
                        'LanguageHaveWorkedWith', 'DatabaseHaveWorkedWith', 'OrgSize')]

# Our goal is to predict ConvertedCompYearly, so just drop it, if it doesn't exist
data_subset <- data_subset %>%
  filter(!is.na(ConvertedCompYearly))

# Count NA's in each column
na_counts <- colSums(is.na(data_subset))
print(na_counts)

# Convert YearsCode the extreme str's to numerical for easier analysis
data_subset <- data_subset %>%
  mutate(YearsCode = case_when(
    YearsCode == "Less than 1 year" ~ "0.5",
    YearsCode == "More than 50 years" ~ "50",
    TRUE ~ as.character(YearsCode)  
  ))

# Convert 'YearsCode' to numeric
data_subset$YearsCode <- as.numeric(data_subset$YearsCode)

# Multivariate imputation by chained equations (MICE)
# We need to One-hot encode the categorical variables, I have just written a custom function
# DatabaseHaveWorkedWith & LanguageHaveWorkedWith are just lists of every programming 
# language the person may know, this isn't great for us we need to convert it
# to something usable, boolean flag of if they know that program, Na if no response
create_binary_columns <- function(data, column_name) {
  # Determine rows that are NA in the specified column
  is_na_rows <- is.na(data[[column_name]])
  
  # Extract all unique items
  all_items <- unique(unlist(strsplit(na.omit(data[[column_name]]), ";")))
  
  # Function to check presence of each item in the row
  check_language_presence <- function(row_data, language) {
    if (is.na(row_data)) {
      return(FALSE)
    } else {
      languages_in_row <- unlist(strsplit(as.character(row_data), ";"))
      return(language %in% languages_in_row)
    }
  }
  
  # Create a new column for each unique item
  for (language in all_items) {
    data[[language]] <- sapply(data[[column_name]], check_language_presence, language = language)
    # Set NA where the original data in the column is NA
    data[is_na_rows, language] <- NA
  }
  
  return(data)
}

# create a df for only our predictors
predictor_data <- data_subset[, c('EdLevel', 'DevType', 'YearsCode', 
                                  'LanguageHaveWorkedWith', 'DatabaseHaveWorkedWith', 
                                  'OrgSize')]

# process all the columns
predictor_data <- create_binary_columns(predictor_data, 'EdLevel')
predictor_data <- create_binary_columns(predictor_data, 'DevType')
predictor_data <- create_binary_columns(predictor_data, 'OrgSize')
predictor_data <- create_binary_columns(predictor_data, 'DatabaseHaveWorkedWith')
predictor_data <- create_binary_columns(predictor_data, 'LanguageHaveWorkedWith')

# Drop the old messy data
predictor_data <- predictor_data[, !colnames(predictor_data) %in% c("EdLevel", "DevType", 
                                "LanguageHaveWorkedWith", "DatabaseHaveWorkedWith", "OrgSize")]

# Clean col names
names(predictor_data) <- gsub("’", "", names(predictor_data))  
names(predictor_data) <- gsub("'", "", names(predictor_data))  
names(predictor_data) <- gsub("[[:punct:]]", "_", names(predictor_data))  
names(predictor_data) <- gsub(" ", "_", names(predictor_data)) 
names(predictor_data) <- gsub("Developer_", "Dev_", names(predictor_data))
names(predictor_data) <- gsub("Engineer_", "Eng_", names(predictor_data))
names(predictor_data) <- gsub("Professional_", "Prof_", names(predictor_data))
names(predictor_data) <- gsub("Specialist_", "Spec_", names(predictor_data))

names(predictor_data) <- make.names(names(predictor_data))

# Setting methods for MICE
# 'logreg' for binary variables, predictive mean matching for numeric
meth <- rep("logreg", ncol(predictor_data))
meth[names(predictor_data) == "YearsCode"] <- "pmm"

# Perform MICE with specified methods and 5 imputed datasets
imputed_data <- mice(predictor_data, method = meth, m = 5, seed = 123)

# Check for convergence 
plot(imputed_data)

