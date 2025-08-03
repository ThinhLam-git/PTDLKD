# ========================================
# SDG Index Analysis in R
# ========================================

# Install and load required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readxl")) install.packages("readxl")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
if (!require("xgboost")) install.packages("xgboost")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")

library(tidyverse)
library(readxl)
library(caret)
library(randomForest)
library(xgboost)
library(ggplot2)

# Read data
df <- read.csv('data/processed/new_dataset.csv')
print(df)

# Display column names
print(colnames(df))

# Note: Vẽ biểu đồ minh họa phương pháp thực hiện

# ========================================
# Minmax scaler 0-100
# ========================================

# Create a copy to avoid affecting the original df
df2 <- df

# Step 1: Filter columns to normalize (all columns except 'Country', 'year', and 'SDG Index Score')
cols_to_scale <- setdiff(colnames(df2), c('Country', 'year', 'SDG Index Score'))

# Step 2: Normalize the filtered columns
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) * 100)
}

# Apply normalization to each column
df2[cols_to_scale] <- lapply(df2[cols_to_scale], normalize)

# Print results
print(head(df2))

# ========================================
# Model for predicting SDG Index Score
# ========================================

df3 <- df2

# ========================================
# Create lag variables
# ========================================

# Note: Lag variables reflect changes in variables over time and country.
# Since time series methods aren't being used, lag variables help incorporate 
# time effects into ML models

# 1. Create lag variables for all columns except 'Country', 'year', 'SDG Index Score'
exclude_cols <- c('Country', 'year', 'SDG Index Score')
lag_cols <- setdiff(colnames(df3), exclude_cols)

# Create lag columns by country
df3 <- df3 %>%
  group_by(Country) %>%
  mutate(across(all_of(lag_cols), 
                ~ lag(., 1),
                .names = "{.col}_lag")) %>%
  ungroup()

# 2. Remove rows with NaN values in any lag column
lagged_cols <- paste0(lag_cols, "_lag")
df3 <- df3 %>% 
  drop_na(all_of(lagged_cols)) %>%
  as.data.frame()

# 3. Encode Country column as numeric
df3$Country_ID <- as.numeric(as.factor(df3$Country)) - 1

print(df3)

# ========================================
# Train-test split
# ========================================

# Note: Split data by year to preserve time characteristics

# Split data into train/test by year
train_data <- df3 %>% filter(year <= 2020)
test_data <- df3 %>% filter(year >= 2021 & year <= 2024)

cat("Number of rows in train:", nrow(train_data), ", test:", nrow(test_data), "\n")

# ========================================
# Models
# ========================================

# Use 3 models: Linear Regression (linear models), 
# Random Forest (Ensemble Bagging), XGBoost (Ensemble Boosting)
# Without GridSearchCV for parameters + Cross Validation yet

# Prepare features and target
X_train <- train_data %>% select(-Country, -year, -`SDG Index Score`)
y_train <- train_data$`SDG Index Score`

# Linear Regression model
model_lr <- lm(`SDG Index Score` ~ ., data = cbind(y_train, X_train))
summary(model_lr)

# Random Forest model - basic version before tuning
model_rf <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 100,
  importance = TRUE
)
print(model_rf)

# XGBoost model - basic version before tuning
model_xgb <- xgboost(
  data = as.matrix(X_train),
  label = y_train,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)
print(model_xgb)

# ========================================
# Model tuning with cross-validation
# ========================================

# Random Forest tuning using caret
set.seed(42)

# Define parameter grid for Random Forest
rf_grid <- expand.grid(
  mtry = c(floor(sqrt(ncol(X_train))), floor(ncol(X_train)/3), floor(ncol(X_train)/2))
)

# Set up train control for cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

# Tune Random Forest model
rf_tuned <- train(
  x = as.data.frame(X_train),
  y = y_train,
  method = "rf",
  tuneGrid = rf_grid,
  trControl = train_control,
  importance = TRUE
)

print(rf_tuned)
best_rf <- rf_tuned$finalModel

# XGBoost tuning
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6, 10),
  eta = c(0.01, 0.1, 0.2),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Tune XGBoost model
xgb_tuned <- train(
  x = as.data.frame(X_train),
  y = y_train,
  method = "xgbTree",
  tuneGrid = xgb_grid,
  trControl = train_control,
  verbose = FALSE
)

print(xgb_tuned)
best_xgb <- xgb_tuned$finalModel

# ========================================
# Model Validation on Test Set
# ========================================

# Prepare test data
X_test <- test_data %>% select(-Country, -year, -`SDG Index Score`)
y_test <- test_data$`SDG Index Score`

# Make predictions on test set
y_pred_lr <- predict(model_lr, newdata = X_test)
y_pred_rf <- predict(best_rf, newdata = X_test)
y_pred_xgb <- predict(best_xgb, newdata = as.matrix(X_test))

# Evaluate models on test set
calculate_metrics <- function(actual, predicted, model_name) {
  mse <- mean((actual - predicted)^2)
  mae <- mean(abs(actual - predicted))
  r2 <- 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2)
  
  cat("---", model_name, "---\n")
  cat("Mean Squared Error (MSE):", round(mse, 5), "\n")
  cat("Mean Absolute Error (MAE):", round(mae, 5), "\n")
  cat("R^2 Score:", round(r2, 5), "\n\n")
  
  return(list(mse = mse, mae = mae, r2 = r2))
}

lr_metrics <- calculate_metrics(y_test, y_pred_lr, "Linear Regression")
rf_metrics <- calculate_metrics(y_test, y_pred_rf, "Random Forest")
xgb_metrics <- calculate_metrics(y_test, y_pred_xgb, "XGBoost")

# Add comments about results
# Why Linear Regression has the lowest results?

# ========================================
# Forecasting
# ========================================

# ========================================
# Forecasting features for Vietnam
# ========================================

# Predict features, including lag variables, create a dataframe with 
# predicted values for columns from 2025-2030

library(stats)

country_of_interest <- "Vietnam"
history_end_year <- 2024
forecast_start_year <- 2025
forecast_end_year <- 2030

target_columns <- colnames(df3 %>% select(-Country, -year, -`SDG Index Score`))

# Filter historical data for Vietnam
vietnam_history_df <- df3 %>% filter(Country == "Vietnam")

# Create dataframe to store forecast results for targets
forecasted_targets_vn <- as.data.frame(matrix(
  nrow = forecast_end_year - forecast_start_year + 1,
  ncol = length(target_columns)
))
colnames(forecasted_targets_vn) <- target_columns
row.names(forecasted_targets_vn) <- forecast_start_year:forecast_end_year

# Forecast each target variable
for (target_col in target_columns) {
  cat("  - Forecasting Target:", target_col, "\n")
  
  # Get historical target data for Vietnam
  target_history <- vietnam_history_df %>% 
    select(year, all_of(target_col)) %>% 
    drop_na()
  
  if (nrow(target_history) > 0) {
    # Use linear regression to fit on years
    tryCatch({
      lm_formula <- as.formula(paste(target_col, "~ year"))
      lr_target_model <- lm(lm_formula, data = target_history)
      
      # Create year data for forecast period
      future_years_df <- data.frame(year = forecast_start_year:forecast_end_year)
      
      # Forecast target values for future
      forecasted_targets_vn[, target_col] <- predict(lr_target_model, newdata = future_years_df)
      
    }, error = function(e) {
      cat("    Error fitting Linear Regression for", target_col, ":", e$message, 
          "This column might not be forecasted.\n")
      forecasted_targets_vn[, target_col] <- NA
    })
  } else {
    cat("    Warning: No historical data for", target_col, "for Vietnam. Column not forecasted.\n")
    forecasted_targets_vn[, target_col] <- NA
  }
}

# Handle columns that couldn't be forecasted
forecasted_targets_vn <- forecasted_targets_vn %>% 
  select(where(~!all(is.na(.))))
target_columns_forecasted <- colnames(forecasted_targets_vn)

cat("\n--- Forecasted Target Results (2025-2030) ---\n")
print(forecasted_targets_vn)
cat("------------------------------\n")

# ========================================
# Forecasting Vietnam SDG Index Score 2025-2030
# ========================================

# Using predicted values, use 3 trained models to predict
# SDG Index Score for Vietnam from 2025-2030

cat("\nPerforming SDG Index Score forecasting using models...\n")

# Predict with Linear Regression
y_pred_lr <- tryCatch({
  predict(model_lr, newdata = forecasted_targets_vn)
}, error = function(e) {
  cat("Error predicting with Linear Regression:", e$message, "\n")
  return(NULL)
})

# Predict with Random Forest
y_pred_rf <- tryCatch({
  predict(best_rf, newdata = forecasted_targets_vn)
}, error = function(e) {
  cat("Error predicting with Random Forest:", e$message, "\n")
  return(NULL)
})

# Predict with XGBoost
y_pred_xgb <- tryCatch({
  predict(best_xgb, newdata = as.matrix(forecasted_targets_vn))
}, error = function(e) {
  cat("Error predicting with XGBoost:", e$message, "\n")
  return(NULL)
})

# Save forecast results
forecasted_results <- data.frame(
  Year = forecast_start_year:forecast_end_year,
  Forecasted_SDG_LR = y_pred_lr,
  Forecasted_SDG_RF = y_pred_rf,
  Forecasted_SDG_XGB = y_pred_xgb
)

cat("\nSDG Index Score forecast results from models:\n")
print(forecasted_results)

# Plot forecast results
ggplot(forecasted_results, aes(x = Year)) +
  geom_line(aes(y = Forecasted_SDG_LR, color = "Linear Regression"), linetype = "solid", size = 1) +
  geom_point(aes(y = Forecasted_SDG_LR, color = "Linear Regression"), shape = 16, size = 3) +
  geom_line(aes(y = Forecasted_SDG_RF, color = "Random Forest"), linetype = "solid", size = 1) +
  geom_point(aes(y = Forecasted_SDG_RF, color = "Random Forest"), shape = 17, size = 3) +
  geom_line(aes(y = Forecasted_SDG_XGB, color = "XGBoost"), linetype = "solid", size = 1) +
  geom_point(aes(y = Forecasted_SDG_XGB, color = "XGBoost"), shape = 18, size = 3) +
  labs(
    x = "Year",
    y = "Forecasted SDG Index Score",
    title = "Forecasted SDG Index Score by Model",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Advantages of this forecasting method:
# - Leverages multi-country data: Models learn patterns from a much larger dataset
#   (global 2000-2020), making them more general and better at handling compared to
#   training on just 25 data points from Vietnam.
# - Uses exogenous variables: The model accounts for the influence of "target" variables
#   believed to be important.

# Challenges and Notes:
# - Accuracy of target variable forecasts: The effectiveness of the entire SDG Index Score
#   forecasting process depends greatly on how accurately we predict the "target" variables
#   for the future (2025-2030). Errors in target forecasts will propagate and affect SDG Index forecasts.
# - Accumulated errors: In the iterative forecasting process, errors from the previous year
#   will affect the next year's forecast. The further into the future (toward 2030), the
#   greater the accumulated error and the wider the confidence interval of the forecast.
# - Assumption of stability: The model assumes that the relationship between variables
#   (learned from 2000-2020 globally) will continue to hold for Vietnam in 2025-2030.
#   Major structural changes in the future could reduce accuracy.

# ========================================
# SHAP Analysis
# ========================================

# This section analyzes how targets affect SDG Index Score and their specific impacts

if (!require("shapr")) install.packages("shapr")
library(shapr)

# Split data for SHAP analysis
train_data_shap <- df2 %>% filter(year <= 2020)
test_data_shap <- df2 %>% filter(year >= 2021 & year <= 2024)

# Prepare data
X_train_shap <- train_data_shap %>% select(-Country, -year, -`SDG Index Score`)
y_train_shap <- train_data_shap$`SDG Index Score`

# Train XGBoost model for SHAP analysis
model_xgb_shap <- xgboost(
  data = as.matrix(X_train_shap),
  label = y_train_shap,
  nrounds = 100,
  objective = "reg:squarederror",
  eta = 0.1,
  verbose = 0
)

# Prepare test data for SHAP
X_test_shap <- test_data_shap %>% select(-Country, -year, -`SDG Index Score`)
y_test_shap <- test_data_shap$`SDG Index Score`

# Note: R's SHAP implementation is different from Python's
# We'll use variable importance and partial dependence plots as alternatives

# Variable importance from XGBoost
importance_matrix <- xgb.importance(model = model_xgb_shap)
print(importance_matrix)

# Plot variable importance
xgb.plot.importance(importance_matrix, top_n = 20)

# For a specific observation (e.g., Vietnam 2024)
# Find index for Vietnam 2024
sample_idx <- which(test_data_shap$Country == "Vietnam" & test_data_shap$year == 2024)
if (length(sample_idx) > 0) {
  sample_idx <- sample_idx[1]  # Take the first match if multiple exist
  
  # Get feature values for this observation
  sample_features <- X_test_shap[sample_idx, ]
  
  # Print feature values
  cat("\nFeature values for Vietnam 2024:\n")
  print(sample_features)
  
  # Partial dependence plots for top features
  top_features <- head(importance_matrix$Feature, 10)
  
  # In R, we'd typically use packages like pdp or ICE for partial dependence plots
  if (!require("pdp")) install.packages("pdp")
  library(pdp)
  
  for (feature in top_features) {
    if (feature %in% colnames(X_train_shap)) {
      cat("\nPartial dependence plot for feature:", feature, "\n")
      # Create PDPs for the top features
      partial_plot <- partial(
        model_xgb_shap, 
        pred.var = feature,
        train = as.matrix(X_train_shap),
        type = "regression"
      )
      plot(partial_plot, main = paste("Partial Dependence Plot for", feature))
    }
  }
}