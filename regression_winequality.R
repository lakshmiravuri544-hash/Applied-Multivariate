#Load the dataset

red_wine <- read.csv("C:/Users/Lakshmi Ravuri/Desktop/winequality/winequality-red.csv", header=T, sep=";")
white_wine <- read.csv("C:/Users/Lakshmi Ravuri/Desktop/winequality/winequality-white.csv", header=T, sep=";")

#variables are independent of each other
#variables are all numeric

#DATA PREPROCESSING
#Explore the datasets

head(red_wine)
head(white_wine)

# Get the structure of the dataset
str(red_wine)
str(white_wine)

# Summary statistics of the dataset
summary(red_wine)
summary(white_wine)

# Dimensions of the dataset (number of rows, number of columns)
dim(red_wine)
dim(white_wine)

#Handle missing values

# Check for missing values
red_wine_missing=colSums(is.na(red_wine))
white_wine_missing=colSums(is.na(white_wine))

red_wine_missing
white_wine_missing

# Identify the numerical columns in the dataset
red_wine_numeric  <- sapply(red_wine, is.numeric)
white_wine_numeric  <- sapply(white_wine, is.numeric)

red_wine_numeric
white_wine_numeric

#Normalizing
# Scale the numerical columns using the scale function
# Select all columns except the last column

red_col_to_scale <- 1:(ncol(red_wine) - 1)
red_wine_to_scle <- red_wine[, red_col_to_scale] 

red_wine_scaled <- scale(red_wine_to_scle)
red_wine_scaled

white_col_to_scale <- 1:(ncol(white_wine) - 1)
white_wine_to_scle <- white_wine[, white_col_to_scale] 

white_wine_scaled <- scale(white_wine_to_scle)
white_wine_scaled

#outlier detection red wine dataset
# Load the required libraries
library(ggplot2)

# Create a boxplot for each variable
boxplot(red_wine_scaled[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")], main = "Individual Variable Boxplot")

#Fixed acidity
# Load the required libraries
library(ggplot2)

# Convert matrix to data frame
red_wine_df <- as.data.frame(red_wine_scaled)

# Check for missing values
missing_values <- sum(is.na(red_wine_df$fixed.acidity))
print(missing_values)

# Check for non-numeric values
non_numeric_values <- sum(!is.numeric(red_wine_df$fixed.acidity))
print(non_numeric_values)

# Create the boxplot
plot <- ggplot(red_wine_df, aes(x = "", y = fixed.acidity)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = "", y = "fixed.acidity")

# Identify outliers
outliers <- boxplot.stats(red_wine_df$fixed.acidity)$out

# Create a separate data frame for outlier labels
outlier_labels <- red_wine_df[red_wine_df$fixed.acidity %in% outliers, ]

# Add text labels to the outliers
plot + geom_text(data = outlier_labels, aes(x = "", y = fixed.acidity, label = rownames(outlier_labels)),
                 vjust = -1, color = "red", size = 3)

# Create a multivariate boxplot
# Convert matrix to data frame
red_wine_df <- as.data.frame(red_wine_scaled)

library(MVN) 
# Perform Royston's test and create a chi-square plot 
mvn(red_wine_df[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")], mvnTest = "royston", multivariatePlot = "qq")

# Test for outliers
# A function to create a chi-square plot
chisquare.plot <- function(x, mark) {
  # x: an n x p data matrix
  # mark: number of extreme points to mark
  # number of variables
  p <- ncol(x)
  # sample size
  n <- nrow(x)
  # xbar and s
  xbar <- colMeans(x)
  s <- cov(x)
  # Mahalanobis dist
  x.cen <- scale(x, center = TRUE, scale = FALSE)
  d2 <- diag(x.cen %*% solve(s) %*% t(x.cen))
  # chi-sq quantiles
  qchi <- qchisq((1:n - 0.5) / n, df = p)
  # sorted d^2 value
  sortd <- sort(d2)
  # plot
  plot(qchi, sortd, pch = 19, xlab = "Chi-square quantiles", ylab = "Mahalanobis squared distance",
       main = "Chi-square Q-Q Plot")
  # Mark the top 'mark' points with highest distance values
  points(qchi[(n - mark + 1):n], sortd[(n - mark + 1):n], cex = 3, col = "#990000")
  
  # Add text labels for outliers
  text(qchi[(n - mark + 1):n], sortd[(n - mark + 1):n], labels = paste((n - mark + 1):n), pos = 3)
  
  # Return the indices of the outliers
  outliers_indices <- order(d2, decreasing = TRUE)[1:mark]
  return(outliers_indices)
}

# Call the function and draw the chi-square plot
par(mfrow = c(1, 1))
marked_indices <- chisquare.plot(x = as.matrix(red_wine_df[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")]), mark = 2)

# Identify the outlier rows
marked_outliers <- red_wine_df[marked_indices, ]

# Adjust the row numbers for labeling
label_indices <- (nrow(red_wine_df) - marked_indices + 1)

# Print outlier numbers
cat("Outlier Numbers:\n")
cat(label_indices, "\n")

# Print top 2 multivariate outliers
cat("Top 2 Multivariate Outliers:\n")
print(marked_outliers)
                               
#outlier detection white wine dataset
# Create a multivariate plot
# Convert matrix to data frame
white_wine_df <- as.data.frame(white_wine_scaled)

library(MVN) 
# Perform mardia's test and create a chi-square plot 
mvn(white_wine_df[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")], mvnTest = "mardia", multivariatePlot = "qq")

# Test for outliers
# A function to create a chi-square plot
chisquare.plot <- function(x, mark) {
  # x: an n x p data matrix
  # mark: number of extreme points to mark
  # number of variables
  p <- ncol(x)
  # sample size
  n <- nrow(x)
  # xbar and s
  xbar <- colMeans(x)
  s <- cov(x)
  # Mahalanobis dist
  x.cen <- scale(x, center = TRUE, scale = FALSE)
  d2 <- diag(x.cen %*% solve(s) %*% t(x.cen))
  # chi-sq quantiles
  qchi <- qchisq((1:n - 0.5) / n, df = p)
  # sorted d^2 value
  sortd <- sort(d2)
  # plot
  plot(qchi, sortd, pch = 19, xlab = "Chi-square quantiles", ylab = "Mahalanobis squared distance",
       main = "Chi-square Q-Q Plot")
  # Mark the top 'mark' points with highest distance values
  points(qchi[(n - mark + 1):n], sortd[(n - mark + 1):n], cex = 3, col = "#990000")
  
  # Add text labels for outliers
  text(qchi[(n - mark + 1):n], sortd[(n - mark + 1):n], labels = paste((n - mark + 1):n), pos = 3)
  
  # Return the indices of the outliers
  outliers_indices <- order(d2, decreasing = TRUE)[1:mark]
  return(outliers_indices)
}

# Call the function and draw the chi-square plot
par(mfrow = c(1, 1))
marked_indices <- chisquare.plot(x = as.matrix(white_wine_df[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")]), mark = 1)

# Identify the outlier rows
marked_outliers <- white_wine_df[marked_indices, ]

# Adjust the row numbers for labeling
label_indices <- (nrow(white_wine_df) - marked_indices + 1)

# Print outlier numbers
cat("Outlier Numbers:\n")
cat(label_indices, "\n")

# Print top 1 multivariate outliers
cat("Top 1 Multivariate Outliers:\n")
print(marked_outliers)

#SCALED DATA
#removing outliers from the data
red_wine_out <- red_wine[-c(152, 259), ]
red_wine_out_scaled <- scale(red_wine_out[, -ncol(red_wine_out)])
r_scaled_data <- cbind(red_wine_out_scaled, red_wine_out[, ncol(red_wine_out)])

#removing outliers from the data
white_wine_out <- white_wine[-c(2782), ]
white_wine_out_scaled <- scale(white_wine_out[, -ncol(white_wine_out)])
w_scaled_data <- cbind(white_wine_out_scaled, white_wine_out[, ncol(white_wine_out)])

scaled_data <- rbind(r_scaled_data, w_scaled_data)

#split the dataset to train and test_val
#Split the dataset into training and test sets

set.seed(142)

red_train_index <- sample.int(nrow(r_scaled_data), 0.8 * nrow(r_scaled_data))
red_train_data <- r_scaled_data [red_train_index, ]
red_test_data <- r_scaled_data [-red_train_index, ]

# Check the dimensions of the resulting sets
dim(red_train_data)
dim(red_test_data)

set.seed(142)

white_train_index <- sample.int(nrow(w_scaled_data), 0.8 * nrow(w_scaled_data))
white_train_data <- w_scaled_data[white_train_index, ]
white_test_data <- w_scaled_data[-white_train_index, ]

# Check the dimensions of the resulting sets
dim(white_train_data)
dim(white_test_data)

#Prepare the data
#Combine the datasets
combined_train_data <- rbind(red_train_data, white_train_data)

# Prepare the training data
X_train <- combined_train_data[, -ncol(combined_train_data)]
y_train <- combined_train_data[, ncol(combined_train_data)]
X_train <- as.data.frame(X_train)

#Split the test data
combined_test_data <- rbind(red_test_data, white_test_data)

#Prepare the test data
X_test <- combined_test_data[, -ncol(combined_test_data)]
y_test <- combined_test_data[, ncol(combined_test_data)]
X_test <- as.data.frame(X_test)

#Checking for multicollinearity
cor_matrix <- cor(X_train, y_train)
print(cor_matrix)

library(ggplot2)
library(reshape2)

# Reshape correlation matrix for heatmap
cor_matrix_melted <- melt(cor_matrix)

# Create heatmap
ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

# Compute correlation matrix for predictor variables
cor_matrix_predictors <- cor(X_train)

# Print correlation matrix
print(cor_matrix_predictors)

# Create correlation heatmap
library(ggplot2)
library(reshape2)

cor_matrix_melted <- melt(cor_matrix_predictors)
cor_matrix_melted

ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

#Multivariate Linear regression
# Train the linear regression model
lm_model <- lm(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(lm_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(lm_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_lm <- mean((y_train_pred - y_train)^2)
rmse_train_lm <- sqrt(mse_train)
mae_train_lm <- mean(abs(y_train_pred - y_train))
r2_train_lm <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_lm <- mean((y_test_pred - y_test)^2)
rmse_test_lm <- sqrt(mse_test)
mae_test_lm <- mean(abs(y_test_pred - y_test))
r2_test_lm <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_lm))
print(paste("RMSE:", rmse_train_lm))
print(paste("MAE:", mae_train_lm))
print(paste("R-squared:", r2_train_lm))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_lm))
print(paste("RMSE:", rmse_test_lm))
print(paste("MAE:", mae_test_lm))
print(paste("R-squared:", r2_test_lm))

#Decision tree
# Train the decision tree model
library(rpart)
tree_model <- rpart(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(tree_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(tree_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_dt <- mean((y_train_pred - y_train)^2)
rmse_train_dt <- sqrt(mse_train)
mae_train_dt <- mean(abs(y_train_pred - y_train))
r2_train_dt <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_dt <- mean((y_test_pred - y_test)^2)
rmse_test_dt <- sqrt(mse_test)
mae_test_dt <- mean(abs(y_test_pred - y_test))
r2_test_dt <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_dt))
print(paste("RMSE:", rmse_train_dt))
print(paste("MAE:", mae_train_dt))
print(paste("R-squared:", r2_train_dt))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_dt))
print(paste("RMSE:", rmse_test_dt))
print(paste("MAE:", mae_test_dt))
print(paste("R-squared:", r2_test_dt))

#Random forest
# Train the random forest model
library(randomForest)
rf_model <- randomForest(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(rf_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(rf_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_rf <- mean((y_train_pred - y_train)^2)
rmse_train_rf <- sqrt(mse_train)
mae_train_rf <- mean(abs(y_train_pred - y_train))
r2_train_rf <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_rf <- mean((y_test_pred - y_test)^2)
rmse_test_rf <- sqrt(mse_test)
mae_test_rf <- mean(abs(y_test_pred - y_test))
r2_test_rf <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_rf))
print(paste("RMSE:", rmse_train_rf))
print(paste("MAE:", mae_train_rf))
print(paste("R-squared:", r2_train_rf))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_rf))
print(paste("RMSE:", rmse_test_rf))
print(paste("MAE:", mae_test_rf))
print(paste("R-squared:", r2_test_rf))

#Support vector regression
# Train the SVR model
library(e1071)
svr_model <- svm(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(svr_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(svr_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_sm <- mean((y_train_pred - y_train)^2)
rmse_train_sm <- sqrt(mse_train)
mae_train_sm <- mean(abs(y_train_pred - y_train))
r2_train_sm <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_sm <- mean((y_test_pred - y_test)^2)
rmse_test_sm <- sqrt(mse_test)
mae_test_sm <- mean(abs(y_test_pred - y_test))
r2_test_sm <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_sm))
print(paste("RMSE:", rmse_train_sm))
print(paste("MAE:", mae_train_sm))
print(paste("R-squared:", r2_train_sm))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_sm))
print(paste("RMSE:", rmse_test_sm))
print(paste("MAE:", mae_test_sm))
print(paste("R-squared:", r2_test_sm))

metrics <- c("MSE", "RMSE", "MAE", "R-squared")
models <- c("MultiLinear Reg", "Decision Tree", "Random Forest", "SVR")

# Create empty matrices to store the evaluation metric values for train and test sets
metrics_values_train <- matrix(0, nrow = length(models), ncol = length(metrics))
metrics_values_test <- matrix(0, nrow = length(models), ncol = length(metrics))

# Assign the evaluation metric values for train set to the matrix
metrics_values_train[1, ] <- c(mse_train_lm, rmse_train_lm, mae_train_lm, r2_train_lm)
metrics_values_train[2, ] <- c(mse_train_dt, rmse_train_dt, mae_train_dt, r2_train_dt)
metrics_values_train[3, ] <- c(mse_train_rf, rmse_train_rf, mae_train_rf, r2_train_rf)
metrics_values_train[4, ] <- c(mse_train_sm, rmse_train_sm, mae_train_sm, r2_train_sm)

# Assign the evaluation metric values for test set to the matrix
metrics_values_test[1, ] <- c(mse_test_lm, rmse_test_lm, mae_test_lm, r2_test_lm)
metrics_values_test[2, ] <- c(mse_test_dt, rmse_test_dt, mae_test_dt, r2_test_dt)
metrics_values_test[3, ] <- c(mse_test_rf, rmse_test_rf, mae_test_rf, r2_test_rf)
metrics_values_test[4, ] <- c(mse_test_sm, rmse_test_sm, mae_test_sm, r2_test_sm)

# Transpose the matrices
metrics_values_train <- t(metrics_values_train)
metrics_values_test <- t(metrics_values_test)

# Create bar plots for train set
barplot(metrics_values_train, beside = TRUE, col = c("steelblue", "#ff7f89", "darkgreen", "orange"), ylim = c(0, 1.0),
        names.arg = models, xlab = "Models", ylab = "Evaluation Metrics", main = "Evaluation Metrics Comparison (Train Set)",
        legend.text = metrics, args.legend = list(x = "topright", bty = "n"))

# Create bar plots for test set
barplot(metrics_values_test, beside = TRUE, col = c("steelblue", "#ff7f89", "darkgreen", "orange"), ylim = c(0, 1.0),
        names.arg = models, xlab = "Models", ylab = "Evaluation Metrics", main = "Evaluation Metrics Comparison (Test Set)",
        legend.text = metrics, args.legend = list(x = "topright", bty = "n"))

#Linear regression for each variable
#Create a list to store the evaluation metrics
metrics_list <- list()

# Iterate over each variable
for (variable in colnames(X_train)) {
  # Train the linear regression model
  lm_model <- lm(y_train ~ ., data = X_train[, variable, drop = FALSE])
  
  # Predict on the training set
  y_train_pred <- predict(lm_model, newdata = X_train[, variable, drop = FALSE])
  
  # Predict on the test set
  y_test_pred <- predict(lm_model, newdata = X_test[, variable, drop = FALSE])
  
  # Calculate evaluation metrics for training set
  mse_train <- mean((y_train_pred - y_train)^2)
  rmse_train <- sqrt(mse_train)
  mae_train <- mean(abs(y_train_pred - y_train))
  r2_train <- summary(lm_model)$r.squared
  
  # Calculate evaluation metrics for test set
  mse_test <- mean((y_test_pred - y_test)^2)
  rmse_test <- sqrt(mse_test)
  mae_test <- mean(abs(y_test_pred - y_test))
  r2_test <- cor(y_test_pred, y_test)^2
  
  # Store the evaluation metrics in the list
  metrics <- list(
    variable = variable,
    mse_train = mse_train,
    rmse_train = rmse_train,
    mae_train = mae_train,
    r2_train = r2_train,
    mse_test = mse_test,
    rmse_test = rmse_test,
    mae_test = mae_test,
    r2_test = r2_test
  )
  metrics_list[[variable]] <- metrics
}

# Print the evaluation metrics for each variable
for (variable in colnames(X_train)) {
  metrics <- metrics_list[[variable]]
  
  cat("Variable:", metrics$variable, "\n")
  cat("Evaluation metrics for training set:\n")
  cat("MSE:", metrics$mse_train, "\n")
  cat("RMSE:", metrics$rmse_train, "\n")
  cat("MAE:", metrics$mae_train, "\n")
  cat("R-squared:", metrics$r2_train, "\n")
  
  cat("Evaluation metrics for test set:\n")
  cat("MSE:", metrics$mse_test, "\n")
  cat("RMSE:", metrics$rmse_test, "\n")
  cat("MAE:", metrics$mae_test, "\n")
  cat("R-squared:", metrics$r2_test, "\n")
  cat("\n")
}

# Create empty vectors to store the metrics
variables <- c()
mse_train <- c()
mse_test <- c()
rmse_train <- c()
rmse_test <- c()
mae_train <- c()
mae_test <- c()
r_squared_train <- c()
r_squared_test <- c()

# Iterate over each variable
for (variable in colnames(X_train)) {
  # Exclude missing or non-finite values
  if (any(is.na(X_train[[variable]])) || any(!is.finite(X_train[[variable]]))) {
    next
  }
  
  # Train the linear regression model
  lm_model <- lm(y_train ~ ., data = X_train[, variable, drop = FALSE])
  
  # Predict on the training set
  y_train_pred <- predict(lm_model, newdata = X_train[, variable, drop = FALSE])
  
  # Predict on the test set
  y_test_pred <- predict(lm_model, newdata = X_test[, variable, drop = FALSE])
  
  # Calculate evaluation metrics for training set
  mse_train_val <- mean((y_train_pred - y_train)^2)
  rmse_train_val <- sqrt(mse_train_val)
  mae_train_val <- mean(abs(y_train_pred - y_train))
  r_squared_train_val <- cor(y_train_pred, y_train)^2
  
  # Calculate evaluation metrics for test set
  mse_test_val <- mean((y_test_pred - y_test)^2)
  rmse_test_val <- sqrt(mse_test_val)
  mae_test_val <- mean(abs(y_test_pred - y_test))
  r_squared_test_val <- cor(y_test_pred, y_test)^2
  
  # Append the metrics to the vectors
  variables <- c(variables, variable)
  mse_train <- c(mse_train, mse_train_val)
  mse_test <- c(mse_test, mse_test_val)
  rmse_train <- c(rmse_train, rmse_train_val)
  rmse_test <- c(rmse_test, rmse_test_val)
  mae_train <- c(mae_train, mae_train_val)
  mae_test <- c(mae_test, mae_test_val)
  r_squared_train <- c(r_squared_train, r_squared_train_val)
  r_squared_test <- c(r_squared_test, r_squared_test_val)
}

# Create a data frame with the metrics
metrics_df <- data.frame(
  Variable = variables,
  MSE_Train = mse_train,
  MSE_Test = mse_test,
  RMSE_Train = rmse_train,
  RMSE_Test = rmse_test,
  MAE_Train = mae_train,
  MAE_Test = mae_test,
  R_squared_Train = r_squared_train,
  R_squared_Test = r_squared_test
)

# Plot the metrics
par(mfrow = c(2, 2))

# MSE
barplot(metrics_df$MSE_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MSE", main = "MSE - Training Set", col = "darkgreen")
barplot(metrics_df$MSE_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MSE", main = "MSE - Test Set", col = "orange")

# RMSE
barplot(metrics_df$RMSE_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "RMSE", main = "RMSE - Training Set", col = "darkgreen")
barplot(metrics_df$RMSE_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "RMSE", main = "RMSE - Test Set", col = "orange")

# MAE
barplot(metrics_df$MAE_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MAE", main = "MAE - Training Set", col = "darkgreen")
barplot(metrics_df$MAE_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MAE", main = "MAE - Test Set", col = "orange")

# R-squared
barplot(metrics_df$R_squared_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "R-squared", main = "R-squared - Training Set", col = "darkgreen")
barplot(metrics_df$R_squared_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "R-squared", main = "R-squared - Test Set", col = "orange")

#PCA(Dimensionality reduction)
#SCALED DATA
#removing outliers from the data
red_wine_out <- red_wine[-c(152, 259), ]
red_wine_out_scaled <- scale(red_wine_out[, -ncol(red_wine_out)])
r_scaled_data <- cbind(red_wine_out_scaled, red_wine_out[, ncol(red_wine_out)])

#removing outliers from the data
white_wine_out <- white_wine[]
white_wine_out_scaled <- scale(white_wine_out[, -ncol(white_wine_out)])
w_scaled_data <- cbind(white_wine_out_scaled, white_wine_out[, ncol(white_wine_out)])

scaled_data <- rbind(r_scaled_data, w_scaled_data)

#perform PCA for scaled wine dataset
pca.cor<-princomp(x = scaled_data, scores = TRUE)
summary(pca.cor, loadings = TRUE, cutoff = 0.3)
pca_result <- plot(pca.cor, type = "lines", main = "Scree plot for wine data")
pca_result
#6 components were taken

pca <- prcomp(scaled_data, scale = F)
y <- scaled_data[, ncol(scaled_data)]

X_pca <- as.data.frame(pca$x[, 1:6])

library(caret)
# Create train-test split
set.seed(42)  # Set a seed for reproducibility
indices <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)
X_train <- X_pca[indices, ]
X_test <- X_pca[-indices, ]
y_train <- y[indices]
y_test <- y[-indices]

dim(X_train)
dim(X_test)
length(y_train)
length(y_test)

#Multivariate Linear regression
# Train the linear regression model
lm_model <- lm(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(lm_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(lm_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_lm <- mean((y_train_pred - y_train)^2)
rmse_train_lm <- sqrt(mse_train)
mae_train_lm <- mean(abs(y_train_pred - y_train))
r2_train_lm <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_lm <- mean((y_test_pred - y_test)^2)
rmse_test_lm <- sqrt(mse_test)
mae_test_lm <- mean(abs(y_test_pred - y_test))
r2_test_lm <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_lm))
print(paste("RMSE:", rmse_train_lm))
print(paste("MAE:", mae_train_lm))
print(paste("R-squared:", r2_train_lm))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_lm))
print(paste("RMSE:", rmse_test_lm))
print(paste("MAE:", mae_test_lm))
print(paste("R-squared:", r2_test_lm))

#Decision tree
# Train the decision tree model
library(rpart)
tree_model <- rpart(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(tree_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(tree_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_dt <- mean((y_train_pred - y_train)^2)
rmse_train_dt <- sqrt(mse_train)
mae_train_dt <- mean(abs(y_train_pred - y_train))
r2_train_dt <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_dt <- mean((y_test_pred - y_test)^2)
rmse_test_dt <- sqrt(mse_test)
mae_test_dt <- mean(abs(y_test_pred - y_test))
r2_test_dt <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_dt))
print(paste("RMSE:", rmse_train_dt))
print(paste("MAE:", mae_train_dt))
print(paste("R-squared:", r2_train_dt))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_dt))
print(paste("RMSE:", rmse_test_dt))
print(paste("MAE:", mae_test_dt))
print(paste("R-squared:", r2_test_dt))

#Random forest
# Train the random forest model
library(randomForest)
rf_model <- randomForest(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(rf_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(rf_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_rf <- mean((y_train_pred - y_train)^2)
rmse_train_rf <- sqrt(mse_train)
mae_train_rf <- mean(abs(y_train_pred - y_train))
r2_train_rf <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_rf <- mean((y_test_pred - y_test)^2)
rmse_test_rf <- sqrt(mse_test)
mae_test_rf <- mean(abs(y_test_pred - y_test))
r2_test_rf <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_rf))
print(paste("RMSE:", rmse_train_rf))
print(paste("MAE:", mae_train_rf))
print(paste("R-squared:", r2_train_rf))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_rf))
print(paste("RMSE:", rmse_test_rf))
print(paste("MAE:", mae_test_rf))
print(paste("R-squared:", r2_test_rf))

#Support vector regression
# Train the SVR model
library(e1071)
svr_model <- svm(y_train ~ ., data = X_train)

# Predict on the training set
y_train_pred <- predict(svr_model, newdata = X_train)

# Predict on the test set
y_test_pred <- predict(svr_model, newdata = X_test)

# Calculate evaluation metrics for training set
mse_train_sm <- mean((y_train_pred - y_train)^2)
rmse_train_sm <- sqrt(mse_train)
mae_train_sm <- mean(abs(y_train_pred - y_train))
r2_train_sm <- cor(y_train_pred, y_train)^2

# Calculate evaluation metrics for test set
mse_test_sm <- mean((y_test_pred - y_test)^2)
rmse_test_sm <- sqrt(mse_test)
mae_test_sm <- mean(abs(y_test_pred - y_test))
r2_test_sm <- cor(y_test_pred, y_test)^2

# Print evaluation metrics for training set
print("Evaluation metrics for training set:")
print(paste("MSE:", mse_train_sm))
print(paste("RMSE:", rmse_train_sm))
print(paste("MAE:", mae_train_sm))
print(paste("R-squared:", r2_train_sm))

# Print evaluation metrics for test set
print("Evaluation metrics for test set:")
print(paste("MSE:", mse_test_sm))
print(paste("RMSE:", rmse_test_sm))
print(paste("MAE:", mae_test_sm))
print(paste("R-squared:", r2_test_sm))

metrics <- c("MSE", "RMSE", "MAE", "R-squared")
models <- c("MultiLinear Reg", "Decision Tree", "Random Forest", "SVR")

# Create empty matrices to store the evaluation metric values for train and test sets
metrics_values_train <- matrix(0, nrow = length(models), ncol = length(metrics))
metrics_values_test <- matrix(0, nrow = length(models), ncol = length(metrics))

# Assign the evaluation metric values for train set to the matrix
metrics_values_train[1, ] <- c(mse_train_lm, rmse_train_lm, mae_train_lm, r2_train_lm)
metrics_values_train[2, ] <- c(mse_train_dt, rmse_train_dt, mae_train_dt, r2_train_dt)
metrics_values_train[3, ] <- c(mse_train_rf, rmse_train_rf, mae_train_rf, r2_train_rf)
metrics_values_train[4, ] <- c(mse_train_sm, rmse_train_sm, mae_train_sm, r2_train_sm)

# Assign the evaluation metric values for test set to the matrix
metrics_values_test[1, ] <- c(mse_test_lm, rmse_test_lm, mae_test_lm, r2_test_lm)
metrics_values_test[2, ] <- c(mse_test_dt, rmse_test_dt, mae_test_dt, r2_test_dt)
metrics_values_test[3, ] <- c(mse_test_rf, rmse_test_rf, mae_test_rf, r2_test_rf)
metrics_values_test[4, ] <- c(mse_test_sm, rmse_test_sm, mae_test_sm, r2_test_sm)

# Transpose the matrices
metrics_values_train <- t(metrics_values_train)
metrics_values_test <- t(metrics_values_test)

# Create bar plots for train set
barplot(metrics_values_train, beside = TRUE, col = c("steelblue", "#ff7f89", "darkgreen", "orange"), ylim = c(0, 1.0),
        names.arg = models, xlab = "Models", ylab = "Evaluation Metrics", main = "Evaluation Metrics Comparison (Train Set)",
        legend.text = metrics, args.legend = list(x = "topright", bty = "n"))

# Create bar plots for test set
barplot(metrics_values_test, beside = TRUE, col = c("steelblue", "#ff7f89", "darkgreen", "orange"), ylim = c(0, 1.0),
        names.arg = models, xlab = "Models", ylab = "Evaluation Metrics", main = "Evaluation Metrics Comparison (Test Set)",
        legend.text = metrics, args.legend = list(x = "topright", bty = "n"))

#Linear regression for each variable
#Create a list to store the evaluation metrics
metrics_list <- list()

# Iterate over each variable
for (variable in colnames(X_train)) {
  # Train the linear regression model
  lm_model <- lm(y_train ~ ., data = X_train[, variable, drop = FALSE])
  
  # Predict on the training set
  y_train_pred <- predict(lm_model, newdata = X_train[, variable, drop = FALSE])
  
  # Predict on the test set
  y_test_pred <- predict(lm_model, newdata = X_test[, variable, drop = FALSE])
  
  # Calculate evaluation metrics for training set
  mse_train <- mean((y_train_pred - y_train)^2)
  rmse_train <- sqrt(mse_train)
  mae_train <- mean(abs(y_train_pred - y_train))
  r2_train <- summary(lm_model)$r.squared
  
  # Calculate evaluation metrics for test set
  mse_test <- mean((y_test_pred - y_test)^2)
  rmse_test <- sqrt(mse_test)
  mae_test <- mean(abs(y_test_pred - y_test))
  r2_test <- cor(y_test_pred, y_test)^2
  
  # Store the evaluation metrics in the list
  metrics <- list(
    variable = variable,
    mse_train = mse_train,
    rmse_train = rmse_train,
    mae_train = mae_train,
    r2_train = r2_train,
    mse_test = mse_test,
    rmse_test = rmse_test,
    mae_test = mae_test,
    r2_test = r2_test
  )
  metrics_list[[variable]] <- metrics
}

# Print the evaluation metrics for each variable
for (variable in colnames(X_train)) {
  metrics <- metrics_list[[variable]]
  
  cat("Variable:", metrics$variable, "\n")
  cat("Evaluation metrics for training set:\n")
  cat("MSE:", metrics$mse_train, "\n")
  cat("RMSE:", metrics$rmse_train, "\n")
  cat("MAE:", metrics$mae_train, "\n")
  cat("R-squared:", metrics$r2_train, "\n")
  
  cat("Evaluation metrics for test set:\n")
  cat("MSE:", metrics$mse_test, "\n")
  cat("RMSE:", metrics$rmse_test, "\n")
  cat("MAE:", metrics$mae_test, "\n")
  cat("R-squared:", metrics$r2_test, "\n")
  cat("\n")
}

# Create empty vectors to store the metrics
variables <- c()
mse_train <- c()
mse_test <- c()
rmse_train <- c()
rmse_test <- c()
mae_train <- c()
mae_test <- c()
r_squared_train <- c()
r_squared_test <- c()

# Iterate over each variable
for (variable in colnames(X_train)) {
  # Exclude missing or non-finite values
  if (any(is.na(X_train[[variable]])) || any(!is.finite(X_train[[variable]]))) {
    next
  }
  
  # Train the linear regression model
  lm_model <- lm(y_train ~ ., data = X_train[, variable, drop = FALSE])
  
  # Predict on the training set
  y_train_pred <- predict(lm_model, newdata = X_train[, variable, drop = FALSE])
  
  # Predict on the test set
  y_test_pred <- predict(lm_model, newdata = X_test[, variable, drop = FALSE])
  
  # Calculate evaluation metrics for training set
  mse_train_val <- mean((y_train_pred - y_train)^2)
  rmse_train_val <- sqrt(mse_train_val)
  mae_train_val <- mean(abs(y_train_pred - y_train))
  r_squared_train_val <- cor(y_train_pred, y_train)^2
  
  # Calculate evaluation metrics for test set
  mse_test_val <- mean((y_test_pred - y_test)^2)
  rmse_test_val <- sqrt(mse_test_val)
  mae_test_val <- mean(abs(y_test_pred - y_test))
  r_squared_test_val <- cor(y_test_pred, y_test)^2
  
  # Append the metrics to the vectors
  variables <- c(variables, variable)
  mse_train <- c(mse_train, mse_train_val)
  mse_test <- c(mse_test, mse_test_val)
  rmse_train <- c(rmse_train, rmse_train_val)
  rmse_test <- c(rmse_test, rmse_test_val)
  mae_train <- c(mae_train, mae_train_val)
  mae_test <- c(mae_test, mae_test_val)
  r_squared_train <- c(r_squared_train, r_squared_train_val)
  r_squared_test <- c(r_squared_test, r_squared_test_val)
}

# Create a data frame with the metrics
metrics_df <- data.frame(
  Variable = variables,
  MSE_Train = mse_train,
  MSE_Test = mse_test,
  RMSE_Train = rmse_train,
  RMSE_Test = rmse_test,
  MAE_Train = mae_train,
  MAE_Test = mae_test,
  R_squared_Train = r_squared_train,
  R_squared_Test = r_squared_test
)

# Plot the metrics
par(mfrow = c(2, 2))

# MSE
barplot(metrics_df$MSE_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MSE", main = "MSE - Training Set", col = "darkgreen")
barplot(metrics_df$MSE_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MSE", main = "MSE - Test Set", col = "orange")

# RMSE
barplot(metrics_df$RMSE_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "RMSE", main = "RMSE - Training Set", col = "darkgreen")
barplot(metrics_df$RMSE_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "RMSE", main = "RMSE - Test Set", col = "orange")

# MAE
barplot(metrics_df$MAE_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MAE", main = "MAE - Training Set", col = "darkgreen")
barplot(metrics_df$MAE_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "MAE", main = "MAE - Test Set", col = "orange")

# R-squared
barplot(metrics_df$R_squared_Train, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "R-squared", main = "R-squared - Training Set", col = "darkgreen")
barplot(metrics_df$R_squared_Test, names.arg = metrics_df$Variable, xlab = "Variable", ylab = "R-squared", main = "R-squared - Test Set", col = "orange")
