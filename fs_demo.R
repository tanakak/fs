#################################################################
# 0. Preparation: Load Packages
#################################################################

# Install required packages (only for the first time)
# 'devtools' is necessary to install packages from GitHub.
# This step will be skipped if it's already installed.
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Force install the latest version of the FactorialSurvey package from GitHub.
# "force = TRUE" ensures that it overwrites any existing version.
# "upgrade = 'never'" prevents the upgrading of other packages.
devtools::install_github("tanakak/FactorialSurvey",
                         force = TRUE,
                         upgrade = "never",
                         dependencies = TRUE)

# Load the FactorialSurvey package.
# This makes functions like fs_betareg available for use.
library(FactorialSurvey)


# Load other necessary packages.
# These are the "tools" required for the data analysis.
library(dplyr)      # For efficient data manipulation and processing.
library(tidyr)      # For tidying data, especially for converting between "wide" and "long" formats.
library(betareg)    # For running beta regression models.
library(margins)    # For calculating marginal effects.



#################################################################
# 1. Data Loading and Preprocessing
#################################################################


# --- Load the Data ---
# Use the read.csv() function to load the CSV file into a data frame in R.
# The data will be stored in an object named "data".
# [NOTE] Please ensure that "bento.csv" is in your current working directory.
data <- read.csv("bento.csv")


# --- Remove Unnecessary Columns ---
# Delete the 'time' column, as it will not be used in the analysis.
data <- data %>% select(-time)


# --- Create Respondent IDs ---
# Add an 'id' column to uniquely identify each row (each respondent).
data$id <- 1:nrow(data)


# --- Reorder Columns ---
# Move the 'id' column to the first position.
data <- data %>% select(id, everything())


# --- Inspect the Data ---
str(data)
summary(data)



#################################################################
# 2. Data Reshaping (from Wide to Long)
#################################################################

# --- Reshape the Data ---
# Convert the data from "wide" format to "long" format.
data_long <- data %>%
  pivot_longer(
    # Specify the columns to be reshaped (all columns starting with "fs_q")
    cols = starts_with("fs_q"),
    
    # The name of the new column that will store the original column names
    names_to = "id_fse",
    
    # The name of the new column that will store the original values
    values_to = "rating"
  ) %>%
  mutate(
    # Remove the "fs_q" prefix from the "id_fse" column and convert it to a number
    id_fse = as.integer(gsub("fs_q", "", id_fse))
  ) %>%
  # Sort the data by respondent id and vignette id
  arrange(id, id_fse)


# --- Check the Reshaped Data ---
head(data_long)



#################################################################
# 3. Merging with Vignette Information
#################################################################

# --- Load FSE Vignette Data ---
# [NOTE] Please ensure that "vignette.csv" is in your current working directory.
data_fse <- read.csv("vignette.csv")


# --- Merge the Datasets ---
# Join 'data_long' and 'data_fse' using "id_fse" as the key.
data_merged <- data_long %>%
  left_join(data_fse, by = "id_fse")


# --- Check the Merged Data ---
str(data_merged)
head(data_merged)



#################################################################
# 4. Factorial Survey Experiment (FSE) Analysis
#################################################################


# --- Create Variables for Analysis ---
# Rename the vignette factors (x1, x2, etc.) to more intuitive names
# and convert them into dummy variables for the analysis.
data_merged <- data_merged %>%
  mutate(
    price        = x4,
    main.chicken = ifelse(x1 == 1, 1, 0),
    main.tofu    = ifelse(x1 == 2, 1, 0),
    side         = ifelse(x2 == 1, 1, 0),
    info         = ifelse(x3 == 1, 1, 0)
  )

# --- Check Descriptive Statistics ---
summary(data_merged %>% select(rating, price, main.chicken, main.tofu, side, info))


# --- Estimate the Model ---
# Use the fs_betareg() function to estimate a beta regression model.
# This function automates model estimation, calculation of marginal effects,
# and printing the results based on the provided formula and data.

result <- fs_betareg(
  # Specify the model formula: explain 'rating' with price, main.chicken, etc.
  formula = rating ~ price + main.chicken + main.tofu + side + info,
  
  # Specify the data frame to be used for the analysis
  data = data_merged,
  
  # Automatically transform the response variable 'rating' to the (0, 1) interval
  # required for beta regression.
  transform = TRUE
)


#################################################################
# 5. Marginal Effects (Original 0-1 Scale vs. Rescaled 0-10 Scale)
#################################################################

# When `verbose=TRUE` (the default), the fs_betareg() function prints both
# the (0-1) scale and (0-10) scale marginal effects to the console during execution.

# Additionally, the returned `result` object stores these results.
# You can also call and display them individually as shown below.

# Average Marginal Effects on the (0-1) scale
cat("\n\n===== Average Marginal Effects (0-1 Scale) =====\n")
# The `result$marginal_effects` object stores the results on the (0-1) scale
print(summary(result$marginal_effects))

# Average Marginal Effects rescaled to the (0-10) scale
cat("\n\n===== Average Marginal Effects (0-10 Scale) =====\n")
# The `result$marginal_effects_rescaled` object stores the rescaled results
print(result$marginal_effects_rescaled)




#################################################################
# 6. Estimating Marginal Willingness to Pay (MWTP)
#################################################################

# The fs_mwtp() function is included in the "FactorialSurvey" package.
# It takes the results from fs_betareg() and automatically calculates
# the MWTP for a specified attribute.

# --- Estimate MWTP for "Main dish is chicken" ---
fs_mwtp(
  model_results = result,
  variable_name = "main.chicken",
  price_variable_name = "price"
)

# --- Estimate MWTP for "A side dish is included" ---
fs_mwtp(
  model_results = result,
  variable_name = "side",
  price_variable_name = "price"
)