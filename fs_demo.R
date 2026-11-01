#################################################################
# FactorialSurvey demo: a lunch-box (bento) factorial survey
#################################################################

# 48 university students each rated 8 lunch-box vignettes on a 0-10 scale
# (0 = "would definitely not buy", 10 = "would definitely buy").
# Each vignette varied four attributes:
#   - x1: Main dish (0 = grilled fish, 1 = teriyaki chicken, 2 = tofu hamburger)
#   - x2: Side dish included (1 = yes, 0 = no)
#   - x3: Nutrition information shown (1 = yes, 0 = no)
#   - x4: Price in yen (700, 900, 1100, 1300)
#
# This script reads the raw survey files, reshapes them, fits a beta
# regression with standard errors clustered by respondent, and computes
# the marginal and expected willingness to pay (WTP).



#################################################################
# 0. Preparation: Load Packages
#################################################################

# Install the FactorialSurvey package from GitHub.
# Run this once, and again whenever you want the latest version.
# 'remotes' is needed to install packages from GitHub.
# "dependencies = TRUE" also installs 'margins', which is used for the
# average marginal effects, and the other suggested packages.
# "upgrade = 'never'" leaves your other packages as they are.
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("tanakak/FactorialSurvey",
                        dependencies = TRUE,
                        upgrade = "never")

# Load the FactorialSurvey package.
library(FactorialSurvey)

# Load other packages used for data handling.
library(dplyr)      # For data manipulation.
library(tidyr)      # For converting between "wide" and "long" formats.



#################################################################
# 1. Data Loading
#################################################################

# --- Load the Data from GitHub ---
# bento.csv    : one row per respondent; columns fs_q1 to fs_q8 hold the
#                ratings of vignettes 1 to 8.
# vignette.csv : one row per vignette; id_fse identifies the vignette and
#                x1 to x4 give its attributes.
# [NOTE] To use your own copies instead, replace the URLs with file names,
#        e.g. read.csv("bento.csv").
base_url  <- "https://raw.githubusercontent.com/tanakak/fs/main/"
data      <- read.csv(paste0(base_url, "bento.csv"))
data_fse  <- read.csv(paste0(base_url, "vignette.csv"))


# --- Create Respondent IDs ---
# Add an 'id' column to identify each row (each respondent).
data$id <- 1:nrow(data)

# Move the 'id' column to the first position.
data <- data %>% select(id, everything())


# --- Inspect the Data ---
str(data)
data_fse



#################################################################
# 2. Data Reshaping (from Wide to Long)
#################################################################

# Convert the data from "wide" format (one row per respondent) to
# "long" format (one row per rating).
data_long <- data %>%
  pivot_longer(
    # The columns to be reshaped (all columns starting with "fs_q")
    cols = starts_with("fs_q"),

    # The new column that will store the original column names
    names_to = "id_fse",

    # The new column that will store the ratings
    values_to = "rating"
  ) %>%
  mutate(
    # Remove the "fs_q" prefix and convert the vignette number to an integer
    id_fse = as.integer(gsub("fs_q", "", id_fse))
  ) %>%
  # Sort by respondent and vignette
  arrange(id, id_fse)

head(data_long)



#################################################################
# 3. Merging with Vignette Information
#################################################################

# Join the ratings and the vignette attributes using "id_fse" as the key.
data_merged <- data_long %>%
  left_join(data_fse, by = "id_fse")

head(data_merged)



#################################################################
# 4. Creating Variables for Analysis
#################################################################

# Give the attributes intuitive names.
# 'main' is a factor: R turns it into dummy variables automatically,
# with the first level (grilled fish) as the reference.
data_merged <- data_merged %>%
  mutate(
    main  = factor(x1, levels = 0:2,
                   labels = c("grilled fish", "teriyaki chicken",
                              "tofu hamburger")),
    side  = x2,
    info  = x3,
    price = x4
  )

summary(data_merged %>% select(rating, main, side, info, price))



#################################################################
# 5. Estimating the Model (Beta Regression)
#################################################################

# fs_betareg() fits a beta regression, which respects the bounds of the
# rating scale. The 0-10 ratings are mapped onto the open interval (0, 1)
# automatically (the defaults are scale = c(0, 10) and transform = TRUE).
#
# Each student rated eight vignettes, so ratings by the same person are
# correlated. "cluster = ~ id" makes the standard errors, and everything
# computed from them (including WTP), robust to this.
result <- fs_betareg(
  rating ~ main + side + info + price,
  data    = data_merged,
  cluster = ~ id
)

# Coefficients with cluster-robust standard errors, fit statistics
# (log-likelihood, AIC, BIC, pseudo R2) and average marginal effects on
# both the (0-1) scale and the original (0-10) rating scale.
summary(result)

# The average marginal effects on the (0-10) scale can also be extracted.
result$marginal_effects_rescaled



#################################################################
# 6. Marginal Willingness to Pay (MWTP)
#################################################################

# fs_mwtp() divides each attribute coefficient by the price coefficient:
# the MWTP is the price change (in yen) that would offset the attribute.
# Without 'variable', all attributes are reported.
mwtp <- fs_mwtp(result, price = "price")
mwtp
plot(mwtp) + ggplot2::labs(x = "Marginal willingness to pay (yen)")

# A single attribute: "A side dish is included"
fs_mwtp(result, variable = "side", price = "price")

# Simulation-based (Krinsky-Robb) intervals instead of the delta method
fs_mwtp(result, price = "price", method = "krinsky-robb", seed = 1)



#################################################################
# 7. Expected Willingness to Pay (EWTP) for Whole Lunch Boxes
#################################################################

# fs_ewtp() values a complete scenario -- a whole lunch box -- rather than
# a single attribute. Give one row per scenario; the price is not needed.
boxes <- data.frame(
  main = c("grilled fish", "teriyaki chicken", "teriyaki chicken"),
  side = c(0, 0, 1),
  info = c(0, 0, 1),
  row.names = c("Fish, plain", "Chicken, plain", "Chicken, full")
)

ewtp <- fs_ewtp(result, boxes, price = "price")
ewtp
plot(ewtp) + ggplot2::labs(x = "Expected willingness to pay (yen)")



#################################################################
# 8. Notes
#################################################################

# The same data are included in the package as 'bento'
# (with 'respondent' in place of 'id'), so the model can also be fitted
# without downloading anything:
#   fit <- fs_betareg(rating ~ main + side + info + price,
#                     data = bento, cluster = ~ respondent)
#
# For yes/no responses, use fs_logit() in place of fs_betareg();
# fs_mwtp() and fs_ewtp() work in the same way.
#
# For a full walk-through, including how to design the vignettes and
# decide how many respondents are needed, see the "Getting started"
# vignette of the package:
#   https://github.com/tanakak/FactorialSurvey/blob/master/vignettes/FactorialSurvey.Rmd
