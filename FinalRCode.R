# Load necessary packages
if (!requireNamespace("glmnet", quietly = TRUE)) {
  install.packages("glmnet")
}
library(glmnet)
if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}
library(pROC)
if (!requireNamespace("ROCR", quietly = TRUE)) {
  install.packages("ROCR")
}
library(ROCR)


if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)


# Read the dataset
cancer_data <- read.csv("breast-cancer-wisconsin.csv")

# Convert 'Class' to binary (0 for 2, 1 for 4)
cancer_data$Class <- as.factor(ifelse(cancer_data$Class == 2, 0, 1))

# Split the data into training and testing sets
set.seed(123)
split_index <- createDataPartition(cancer_data$Class, p = 0.7, list = FALSE)
train_data <- cancer_data[split_index, ]
test_data <- cancer_data[-split_index, ]

# Logistic Regression Model
model <- glm(Class ~ ., data = train_data, family = "binomial")

# Predictions on test set
predictions <- predict(model, newdata = test_data, type = "response")

# ROC Curve
roc_curve <- roc(test_data$Class, predictions)
auc_value <- auc(roc_curve)

# Plot ROC Curve
roccurve <- ggroc(roc_curve, col = "blue", lwd = 2) +
  ggtitle("ROC Curve") +
  theme_minimal()

# Confusion Matrix
predictions_binary <- ifelse(predictions > 0.5, 1, 0)
conf_matrix <- table(Actual = test_data$Class, Predicted = predictions_binary)
conf_matrix

# Confusion Matrix Plot
library(ggplot2)
conf_matrix_df <- as.data.frame(as.table(conf_matrix))
confmatrix = ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted")

# Sensitivity and Specificity
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")


#Save plots

project_folder = "/Users/rohan/Desktop/StatsInEcon"

ggsave(file.path(project_folder, "roccurve.png"), plot = roccurve)
ggsave(file.path(project_folder, "confmatrix.png"), plot = confmatrix)

