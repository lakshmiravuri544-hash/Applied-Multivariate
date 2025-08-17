#Load the dataset

red_wine <- read.csv("C:/Users/Lakshmi Ravuri/Desktop/winequality/winequality-red.csv", header=T, sep=";")
white_wine <- read.csv("C:/Users/Lakshmi Ravuri/Desktop/winequality/winequality-white.csv", header=T, sep=";")

red_wine
white_wine

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

#mosaic plot
# Load the necessary libraries
library(ggplot2)
library(dplyr)

# Combine the wine datasets with labels
red_wine$wine_type <- "Red Wine"
white_wine$wine_type <- "White Wine"
combined_data <- bind_rows(red_wine, white_wine)

# Create a mosaic plot using ggplot2
ggplot(combined_data, aes(x = quality, fill = wine_type)) +
  geom_bar(position = "fill") +
  labs(x = "Quality", y = "Proportion", fill = "Wine Type") +
  ggtitle("Wine Type vs Quality")

quality.unique <- unique(combined_data$quality)
#5 6 7 4 8 3 9

#parallel coordinate plot
library(ggplot2)

# Combine the red and white wine datasets
combined_data <- rbind(red_wine, white_wine)

# Create a new column indicating the wine type
combined_data$wine_type <- ifelse(combined_data$wine_type == "Red Wine", "Red", "White")

# Normalize the numeric variables in the dataset
normalized_data <- as.data.frame(lapply(combined_data[, 1:12], function(x) (x - min(x)) / (max(x) - min(x))))

# Add the wine type column to the normalized data
normalized_data$wine_type <- combined_data$wine_type

# Reshape the data to long format
normalized_data_long <- tidyr::pivot_longer(normalized_data, cols = -wine_type, names_to = "Variable", values_to = "Value")

# Create the parallel coordinates plot
ggplot(normalized_data_long, aes(x = Variable, y = Value, color = wine_type, group = wine_type)) +
  geom_line(alpha = 0.5) +
  labs(x = "Variable", y = "Normalized Value", color = "Wine Type") +
  theme_minimal()

#Line plot
library(ggplot2)
library(reshape2)

# Convert the means matrix to a data frame
means_df <- data.frame(means)
means_df <- melt(means_df, id.vars = "wine_type", variable.name = "Variable", value.name = "Mean")

# Create the star plot using ggplot2
ggplot(means_df, aes(x = Variable, y = Mean, group = wine_type, color = wine_type, fill = wine_type)) +
  geom_polygon(alpha = 0.3) +
  geom_path() +
  theme_minimal() +
  labs(title = "Wine Characteristics", x = "Variable", y = "Mean") +
  scale_color_manual(values = c("#FF0000", "#0000FF")) +
  scale_fill_manual(values = c("#FF0000", "#0000FF"))

#box plot
library(tidyverse)

# Combine the red and white wine datasets
combined_data <- bind_rows(
  red_wine %>% mutate(wine_type = "Red"),
  white_wine %>% mutate(wine_type = "White")
)

# Reshape the data to long format
combined_data_long <- combined_data %>%
  pivot_longer(cols = -c(wine_type, quality), names_to = "variable", values_to = "value")

# Plot boxplots for each variable
ggplot(combined_data_long, aes(x = variable, y = value, fill = wine_type)) +
  geom_boxplot() +
  facet_wrap(~ wine_type, nrow = 1) +
  labs(x = "Variable", y = "Value", fill = "Wine Type") +
  theme_minimal()

#Normalizing
# Scale the numerical columns using the scale function
# Select all columns
#removing outliers 

red_wine <- red_wine[, -ncol(red_wine)]
white_wine <- white_wine [, -ncol(white_wine)]

red_wine_out <- red_wine[-c(152, 259), ]
red_wine_scaled <- scale(red_wine_out)
red_wine_scaled

white_wine_out <- white_wine[-c(2782), ]
white_wine_scaled <- scale(white_wine_out)
white_wine_scaled

#split datasets into 80 and 20 percent and give y as 1 for red and 0 for white
#split the dataset to train and test sets

set.seed(142)

dim(red_wine_scaled)

red_train_index <- sample.int(nrow(red_wine_scaled), 0.8 * nrow(red_wine_scaled))
red_train_data <- red_wine_scaled [red_train_index, ]
red_test_data <- red_wine_scaled [-red_train_index, ]

# Check the dimensions of the resulting sets
dim(red_train_data)
dim(red_test_data)

y_red_train_data <- rep(0,1277)
y_red_test_data <- rep(0,320)

set.seed(142)

white_train_index <- sample.int(nrow(white_wine_scaled), 0.8 * nrow(white_wine_scaled))
white_train_data <- white_wine_scaled[white_train_index, ]
white_test_data <- white_wine_scaled[-white_train_index, ]

# Check the dimensions of the resulting sets
dim(white_train_data)
dim(white_test_data)

y_white_train_data <- rep(1,3917)
y_white_test_data <- rep(1,980)

#pie chart
# Calculate the frequencies of red and white wine
red_freq <- length(y_red_train_data) + length(y_red_test_data)
white_freq <- length(y_white_train_data) + length(y_white_test_data)

# Create a data frame for the pie chart
pie_data <- data.frame(Label = c("Red Wine", "White Wine"),
                       Frequency = c(red_freq, white_freq))

# Define custom colors for the pie chart
custom_colors <- c("red", "blue")

# Plot the pie chart
pie_chart <- ggplot(pie_data, aes(x = "", y = Frequency, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Wine Type") +
  guides(fill = guide_legend(title = "Wine Type")) +
  scale_fill_manual(values = custom_colors)

# Display the pie chart
print(pie_chart)

#X_train_data, y_train
X_train_data <- rbind(red_train_data,white_train_data)
X_train <- as.data.frame(X_train_data)
y_train <- c(y_red_train_data, y_white_train_data)

#X_test_data, y_test
X_test_data <- rbind(red_test_data,white_test_data)
X_test <- as.data.frame(X_test_data)
y_test <- c(y_red_test_data, y_white_test_data)

#Classification models
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(class)
library(naivebayes)
library(rpart)

# Define the weights
weights <- ifelse(y_train == 0, 3, 1)  # Assign weight 3 to class 0 (red) and weight 1 to class 1 (white)

# Train the models
model_rf <- randomForest(x = X_train, y = as.factor(y_train), weights = weights) 
model_gb <- gbm(y_train ~ ., data = cbind(X_train, y_train), distribution = "bernoulli", weights = weights)
model_svm <- svm(x = X_train, y = as.factor(y_train), weights = weights)
model_nb <- naiveBayes(x = X_train, y = as.factor(y_train), weights = weights)
model_knn <- knn(train = X_train, test = X_test, cl = y_train, k = 5)
model_dt <- rpart(as.factor(y_train) ~ ., data = cbind(X_train, y_train), weights = weights)

# Predict on the test set
predictions_rf <- predict(model_rf, newdata = X_test)
predictions_gb <- predict(model_gb, newdata = X_test, n.trees = 100, type = "response")
predictions_svm <- predict(model_svm, newdata = X_test)
predictions_nb <- predict(model_nb, newdata = X_test)
predictions_dt <- predict(model_dt, newdata = X_test, type = "class")

# Convert predictions to binary classes (0 or 1)
predictions_gb <- ifelse(predictions_gb > 0.5, 1, 0)

# Convert predictions and y_test to factors
predictions_rf <- factor(predictions_rf)
predictions_gb <- factor(predictions_gb)
predictions_svm <- factor(predictions_svm)
predictions_nb <- factor(predictions_nb)
predictions_dt <- factor(predictions_dt)
y_test <- factor(y_test)

# Calculate confusion matrix
conf_mat_rf <- confusionMatrix(predictions_rf, y_test)
conf_mat_gb <- confusionMatrix(predictions_gb, y_test)
conf_mat_svm <- confusionMatrix(predictions_svm, y_test)
conf_mat_nb <- confusionMatrix(predictions_nb, y_test)
conf_mat_knn <- confusionMatrix(model_knn, y_test)
conf_mat_dt <- confusionMatrix(predictions_dt, y_test)

# Define a function to calculate F1 score
calculate_f1_score <- function(precision, recall) {
  return(2 * (precision * recall) / (precision + recall))
}

accuracy_rf <- conf_mat_rf$overall["Accuracy"]
precision_rf <- conf_mat_rf$byClass["Precision"]
recall_rf <- conf_mat_rf$byClass["Recall"]
f1_score_rf <- calculate_f1_score(precision_rf, recall_rf)

accuracy_gb <- conf_mat_gb$overall["Accuracy"]
precision_gb <- conf_mat_gb$byClass["Precision"]
recall_gb <- conf_mat_gb$byClass["Recall"]
f1_score_gb <- calculate_f1_score(precision_gb, recall_gb)

accuracy_svm <- conf_mat_svm$overall["Accuracy"]
precision_svm <- conf_mat_svm$byClass["Precision"]
recall_svm <- conf_mat_svm$byClass["Recall"]
f1_score_svm <- calculate_f1_score(precision_svm, recall_svm)

accuracy_nb <- conf_mat_nb$overall["Accuracy"]
precision_nb <- conf_mat_nb$byClass["Precision"]
recall_nb <- conf_mat_nb$byClass["Recall"]
f1_score_nb <- calculate_f1_score(precision_nb, recall_nb)

accuracy_knn <- conf_mat_knn$overall["Accuracy"]
precision_knn <- conf_mat_knn$byClass["Precision"]
recall_knn <- conf_mat_knn$byClass["Recall"]
f1_score_knn <- calculate_f1_score(precision_knn, recall_knn)

accuracy_dt <- conf_mat_dt$overall["Accuracy"]
precision_dt <- conf_mat_dt$byClass["Precision"]
recall_dt <- conf_mat_dt$byClass["Recall"]
f1_score_dt <- calculate_f1_score(precision_dt, recall_dt)

# Create a data frame with the evaluation metrics
models <- c("Random Forest", "Gradient Boost", "SVM", "Naive Bayes", "KNN", "Decision Tree")
accuracy <- c(accuracy_rf, accuracy_gb, accuracy_svm, accuracy_nb, accuracy_knn, accuracy_dt)
precision <- c(precision_rf, precision_gb, precision_svm, precision_nb, precision_knn, precision_dt)
recall <- c(recall_rf, recall_gb, recall_svm, recall_nb, recall_knn, recall_dt)
f1_score <- c(f1_score_rf, f1_score_gb, f1_score_svm, f1_score_nb, f1_score_knn, f1_score_dt)

metrics_df <- data.frame(models, accuracy, precision, recall, f1_score)

# Create plots
library(ggplot2)

# Accuracy
accuracy_plot <- ggplot(metrics_df, aes(x = models, y = accuracy)) +
  geom_bar(stat = "identity", fill = "#17becf") +
  labs(title = "Model Comparison: Accuracy", x = "Model", y = "Accuracy") +
  theme_bw()

# Precision
precision_plot <- ggplot(metrics_df, aes(x = models, y = precision)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Model Comparison: Precision", x = "Model", y = "Precision") +
  theme_bw()

# Recall
recall_plot <- ggplot(metrics_df, aes(x = models, y = recall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Model Comparison: Recall", x = "Model", y = "Recall") +
  theme_bw()

# F1 Score
f1_score_plot <- ggplot(metrics_df, aes(x = models, y = f1_score)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Model Comparison: F1 Score", x = "Model", y = "F1 Score") +
  theme_bw()

# Display the plots
print(accuracy_plot)
print(precision_plot)
print(recall_plot)
print(f1_score_plot)

#PCA(Dimensionality reduction)
#SCALED DATA
#removing outliers from the data
red_wine_out <- red_wine[-c(152, 259), ]
red_wine_out_scaled <- scale(red_wine_out)
dim(red_wine_scaled)
y_red <- rep(0,1597)

#removing outliers from the data
white_wine_out <- white_wine[]
white_wine_out_scaled <- scale(white_wine_out)
dim(white_wine_scaled)
y_white <- rep(1,4898)

scaled_data <- rbind(red_wine_out_scaled, white_wine_out_scaled)
y <- c(y_red,y_white)

#perform PCA for scaled wine dataset
pca.cor<-princomp(x = scaled_data, scores = TRUE)
summary(pca.cor, loadings = TRUE, cutoff = 0.3)
pca_result <- plot(pca.cor, type = "lines", main = "Scree plot for wine data")
pca_result
#6 components were taken

pca <- prcomp(scaled_data, scale = F)

X_pca <- as.data.frame(pca$x[, 1:6])
y_pca <- as.data.frame(y)

# Combine the PCA-transformed data and the labels
combined_data <- cbind(X_pca, y_pca)

# Set the seed for reproducibility
set.seed(42)

# Determine the number of data points
n <- nrow(combined_data)

# Define the proportions for train, test, and validation sets
train_prop <- 0.8
test_prop <- 0.20

# Calculate the number of data points for each set
train_size <- round(n * train_prop)
test_size <- round(n * test_prop)

# Randomly assign indices to each set
train_indices <- sample(seq_len(n), size = train_size)
remaining_indices <- setdiff(seq_len(n), train_indices)
test_indices <- sample(remaining_indices, size = test_size)

# Split the data based on the indices
X_train <- X_pca[train_indices, ]
y_train <- y_pca[train_indices, ]
X_test <- X_pca[test_indices, ]
y_test <- y_pca[test_indices, ]  

dim(X_train)
dim(X_test)
length(y_train)
length(y_test)

# Convert X_train,X_test,X_val to a numeric matrix
X_train <- as.matrix(X_train)
X_test <- as.matrix(X_test)

# Replace empty, NaN, Inf values with 0 in X_train
X_train[is.na(X_train)] <- 0
X_train[is.nan(X_train)] <- 0
X_train[is.infinite(X_train)] <- 0

# Replace empty, NaN, Inf values with 0 in X_test
X_test[is.na(X_test)] <- 0
X_test[is.nan(X_test)] <- 0
X_test[is.infinite(X_test)] <- 0

X_train <- as.data.frame(X_train)
X_test <- as.data.frame(X_test)

#Classification models with PCA
#Classification models
library(caret)
library(randomForest)
library(gbm)
library(e1071)
library(class)
library(naivebayes)
library(rpart)

# Define the weights
weights <- ifelse(y_train == 0, 3, 1)  # Assign weight 3 to class 0 (red) and weight 1 to class 1 (white)

# Train the models
model_rf <- randomForest(x = X_train, y = as.factor(y_train), weights = weights) 
model_gb <- gbm(y_train ~ ., data = cbind(X_train, y_train), distribution = "bernoulli", weights = weights)
model_svm <- svm(x = X_train, y = as.factor(y_train), weights = weights)
model_nb <- naiveBayes(x = X_train, y = as.factor(y_train), weights = weights)
model_knn <- knn(train = X_train, test = X_test, cl = y_train, k = 5)
model_dt <- rpart(as.factor(y_train) ~ ., data = cbind(X_train, y_train), weights = weights)

# Predict on the test set
predictions_rf <- predict(model_rf, newdata = X_test)
predictions_gb <- predict(model_gb, newdata = X_test, n.trees = 100, type = "response")
predictions_svm <- predict(model_svm, newdata = X_test)
predictions_nb <- predict(model_nb, newdata = X_test)
predictions_dt <- predict(model_dt, newdata = X_test, type = "class")

# Convert predictions to binary classes (0 or 1)
predictions_gb <- ifelse(predictions_gb > 0.5, 1, 0)

# Convert predictions and y_test to factors
predictions_rf <- factor(predictions_rf)
predictions_gb <- factor(predictions_gb)
predictions_svm <- factor(predictions_svm)
predictions_nb <- factor(predictions_nb)
predictions_dt <- factor(predictions_dt)
y_test <- factor(y_test)

# Calculate confusion matrix
conf_mat_rf <- confusionMatrix(predictions_rf, y_test)
conf_mat_gb <- confusionMatrix(predictions_gb, y_test)
conf_mat_svm <- confusionMatrix(predictions_svm, y_test)
conf_mat_nb <- confusionMatrix(predictions_nb, y_test)
conf_mat_knn <- confusionMatrix(model_knn, y_test)
conf_mat_dt <- confusionMatrix(predictions_dt, y_test)

# Define a function to calculate F1 score
calculate_f1_score <- function(precision, recall) {
  return(2 * (precision * recall) / (precision + recall))
}

accuracy_rf <- conf_mat_rf$overall["Accuracy"]
precision_rf <- conf_mat_rf$byClass["Precision"]
recall_rf <- conf_mat_rf$byClass["Recall"]
f1_score_rf <- calculate_f1_score(precision_rf, recall_rf)

accuracy_gb <- conf_mat_gb$overall["Accuracy"]
precision_gb <- conf_mat_gb$byClass["Precision"]
recall_gb <- conf_mat_gb$byClass["Recall"]
f1_score_gb <- calculate_f1_score(precision_gb, recall_gb)

accuracy_svm <- conf_mat_svm$overall["Accuracy"]
precision_svm <- conf_mat_svm$byClass["Precision"]
recall_svm <- conf_mat_svm$byClass["Recall"]
f1_score_svm <- calculate_f1_score(precision_svm, recall_svm)

accuracy_nb <- conf_mat_nb$overall["Accuracy"]
precision_nb <- conf_mat_nb$byClass["Precision"]
recall_nb <- conf_mat_nb$byClass["Recall"]
f1_score_nb <- calculate_f1_score(precision_nb, recall_nb)

accuracy_knn <- conf_mat_knn$overall["Accuracy"]
precision_knn <- conf_mat_knn$byClass["Precision"]
recall_knn <- conf_mat_knn$byClass["Recall"]
f1_score_knn <- calculate_f1_score(precision_knn, recall_knn)

accuracy_dt <- conf_mat_dt$overall["Accuracy"]
precision_dt <- conf_mat_dt$byClass["Precision"]
recall_dt <- conf_mat_dt$byClass["Recall"]
f1_score_dt <- calculate_f1_score(precision_dt, recall_dt)

# Create a data frame with the evaluation metrics
models <- c("Random Forest", "Gradient Boost", "SVM", "Naive Bayes", "KNN", "Decision Tree")
accuracy <- c(accuracy_rf, accuracy_gb, accuracy_svm, accuracy_nb, accuracy_knn, accuracy_dt)
precision <- c(precision_rf, precision_gb, precision_svm, precision_nb, precision_knn, precision_dt)
recall <- c(recall_rf, recall_gb, recall_svm, recall_nb, recall_knn, recall_dt)
f1_score <- c(f1_score_rf, f1_score_gb, f1_score_svm, f1_score_nb, f1_score_knn, f1_score_dt)

metrics_df <- data.frame(models, accuracy, precision, recall, f1_score)

# Create plots
library(ggplot2)

# Accuracy
accuracy_plot <- ggplot(metrics_df, aes(x = models, y = accuracy)) +
  geom_bar(stat = "identity", fill = "#17becf") +
  labs(title = "Model Comparison: Accuracy", x = "Model", y = "Accuracy") +
  theme_bw()

# Precision
precision_plot <- ggplot(metrics_df, aes(x = models, y = precision)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Model Comparison: Precision", x = "Model", y = "Precision") +
  theme_bw()

# Recall
recall_plot <- ggplot(metrics_df, aes(x = models, y = recall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Model Comparison: Recall", x = "Model", y = "Recall") +
  theme_bw()

# F1 Score
f1_score_plot <- ggplot(metrics_df, aes(x = models, y = f1_score)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Model Comparison: F1 Score", x = "Model", y = "F1 Score") +
  theme_bw()

# Display the plots
print(accuracy_plot)
print(precision_plot)
print(recall_plot)
print(f1_score_plot)

