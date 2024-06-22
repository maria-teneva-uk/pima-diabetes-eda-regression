library(tidyverse)
library(corrplot)
library(mice)
library(car)
library(Matrix)
library(glmnet)
library(Matrix)
library(naniar)
library(car)
library(caret)
library(VIM)
library(pROC)
#Load datasets
d <- read_csv("C:/Users/Maria/Desktop/Data Science/Stats and ML/Report 20.11/PimaDiabetes.csv")
ToPredict <- read_csv("C:/Users/Maria/Desktop/Data Science/Stats and ML/Report 20.11/ToPredict.csv")

#Check for missing observations in data
colSums(is.na(d))

#Check variable types and structure
variable_types <- data.frame(Variable = names(d), Type = sapply(d, function(x) class(x)))
print(variable_types)
str(d)
#Recode 0 of blood pressure, bmi, skin thickness, glucose, insulin into NA
d$BMI[d$BMI == 0] <- NA
d$BloodPressure[d$BloodPressure == 0] <- NA
d$SkinThickness[d$SkinThickness == 0] <- NA
d$Insulin[d$Insulin == 0] <- NA
d$Glucose[d$Glucose == 0] <- NA

#summary statistics
summary(d)

#NA count by variable
colSums(is.na(d))

# Missing Completely at Random (MCAR) test 
mcar_test_result <- mcar_test(d)
print(mcar_test_result)
#p-value: 4.26e-10, indicating that missing values
#are not at random

#Boxlots to identify outliers-better to do it before 
#imputation as outliers can impact it
selected_variables <- colnames(d)[!colnames(d) %in% c("Outcome")]
boxplots <- lapply(selected_variables, function(variable) {
  ggplot(d, aes(x = 1, y = !!sym(variable))) +  
    geom_boxplot(fill = "violet") +
    labs(title = variable) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),  # Hide x-axis labels
          axis.title.x = element_blank(),  # Hide x-axis title
          legend.position = "none")  # Hide the legend
})


# Combine the boxplots into one plot with facets
combined_plot <- cowplot::plot_grid(plotlist = boxplots, ncol = 4, align = "v")

# Print the combined plot
print(combined_plot)

#bar of outcome
ggplot(d, aes(x = factor(Outcome), fill = factor(Outcome))) +
  geom_bar(width = 0.5)+
  scale_x_discrete(labels = c("Non-Diabetic", "Diabetic")) +
  scale_fill_manual(values = c("blue", "violet")) +  # Set fill colors
  labs(x = "Outcome", y = "Count", title = "Bar Plot of Diabetic vs. Non-Diabetic") +
  theme_minimal() + theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = 16),  
    axis.title.y = element_text(size = 14), 
    axis.title.x = element_text(size = 14),  
    axis.text = element_text(size = 12)  
  )

#Handling outliers without data loss
#Pregnancies
Q1p <- quantile(d$Pregnancies, 0.25)
Q3p <- quantile(d$Pregnancies, 0.75)
IQRp <- Q3p - Q1p
lower_bound_p <- Q1p - 1.5 * IQRp
upper_bound_p <- Q3p + 1.5 * IQRp
upper_bound_p <-13
d$Pregnancies[d$Pregnancies > upper_bound_p] <- upper_bound_p

#BP
Q1bp <- quantile(d$BloodPressure, 0.25,  na.rm=TRUE)
Q3bp <- quantile(d$BloodPressure, 0.75,  na.rm=TRUE)
IQRbp <- Q3bp - Q1bp
lower_bound_bp <- round(Q1bp - 1.5 * IQRbp)
upper_bound_bp <- round(Q3bp + 1.5 * IQRbp)
d$BloodPressure[d$BloodPressure > upper_bound_bp] <- upper_bound_bp
d$BloodPressure[d$BloodPressure < lower_bound_bp] <- lower_bound_bp

#SkinThickness
Q1s <- quantile(d$SkinThickness, 0.25,  na.rm=TRUE)
Q3s <- quantile(d$SkinThickness, 0.75,  na.rm=TRUE)
IQRs <- Q3s - Q1s
lower_bound_s <- round(Q1s - 1.5 * IQRs)
upper_bound_s <- round(Q3s + 1.5 * IQRs)
d$SkinThickness[d$SkinThickness > upper_bound_s] <- upper_bound_s
d$SkinThickness[d$SkinThickness < lower_bound_s] <- lower_bound_s

#Glucose
Q1g <- quantile(d$Glucose, 0.25,  na.rm=TRUE)
Q3g <- quantile(d$Glucose, 0.75,  na.rm=TRUE)
IQRg <- Q3g - Q1g
lower_bound_g <- round(Q1g - 1.5 * IQRg)
upper_bound_g <- round(Q3g + 1.5 * IQRg)
d$Glucose[d$Glucose > upper_bound_g] <- upper_bound_g
d$Glucose[d$Glucose < lower_bound_g] <- lower_bound_g

#Insulin
Q1i <- quantile(d$Insulin, 0.25,  na.rm=TRUE)
Q3i <- quantile(d$Insulin, 0.75,  na.rm=TRUE)
IQRi <- Q3i - Q1i
lower_bound_i <- round(Q1i - 1.5 * IQRi)
upper_bound_i <- round(Q3i + 1.5 * IQRi)
d$Insulin[d$Insulin > upper_bound_i] <- upper_bound_i
d$Insulin[d$Insulin < lower_bound_i] <- lower_bound_i

#BMI
Q1b <- quantile(d$BMI, 0.25,  na.rm=TRUE)
Q3b <- quantile(d$BMI, 0.75,  na.rm=TRUE)
IQRb <- Q3b - Q1b
lower_bound_b <- Q1b - 1.5 * IQRb
upper_bound_b <- Q3b + 1.5 * IQRb
d$BMI[d$BMI > upper_bound_b] <- upper_bound_b
d$BMI[d$BMI < lower_bound_b] <- lower_bound_b

#Diabetes Pedigree
Q1dp <- quantile(d$DiabetesPedigree, 0.25)
Q3dp <- quantile(d$DiabetesPedigree, 0.75)
IQRdp <- Q3dp - Q1dp
lower_bound_dp <- Q1dp - 1.5 * IQRdp
upper_bound_dp <- Q3dp + 1.5 * IQRdp
d$DiabetesPedigree[d$DiabetesPedigree > upper_bound_dp] <- upper_bound_dp
d$DiabetesPedigree[d$DiabetesPedigree < lower_bound_dp] <- lower_bound_dp 

#Age
Q1a <- quantile(d$Age, 0.25)
Q3a <- quantile(d$Age, 0.75)
IQRa <- Q3a - Q1a
lower_bound_a <- round(Q1a - 1.5 * IQRa)
upper_bound_a <- round(Q3a + 1.5 * IQRa)
d$Age[d$Age > upper_bound_a] <- upper_bound_a
d$Age[d$Age < lower_bound_a] <- lower_bound_a
#MICE imputation
data <- d

imp <- mice(data, method = "pmm", m = 15)

# Complete the imputation
imputed_data <- complete(imp)
summary(d)
summary(imputed_data)
#density plots+histograms
density_plots <- lapply(selected_variables, function(variable) {
  ggplot(imputed_data, aes(x = !!sym(variable))) +
    geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity", fill = "violet", color = "white") +
    geom_density(alpha = 0.5, color = "blue") +
    labs(title = paste("Distribution of", variable),
         x = variable,
         y = "Density") +
    theme_minimal()
})
# Combine the density plots into one plot
combined_density_plot <- cowplot::plot_grid(plotlist = density_plots, ncol = 4, align = "v")

# Print the combined density plot
print(combined_density_plot)
# Compute the correlation matrix
numerical_columns <- c("Age","Insulin", "Glucose", "BloodPressure", "BMI", "SkinThickness", "DiabetesPedigree", "Pregnancies")
cor_matrix <- cor(imputed_data[, numerical_columns])
# Customize and plot the correlation matrix
blue_violet <- colorRampPalette(c("blue","violet"))(50)
# Customize and plot the correlation matrix with the defined color scale
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, addCoef.col = "black",
         col = blue_violet) 



#Task 3
#Add a column 7 or more pregnancies
d$SevenOrMorePregnancies <- ifelse(d$Pregnancies >= 7, 1, 0)
#Call logistic regression - it doesn't matter whether we do
#it on imputed data or original data since
#there were no missing values in pregnancies and outcome
#and the cutoff point for IQR outliers is 13 
#glm fits for the positive outcome (1=diabetes)
model <- glm(Outcome ~ SevenOrMorePregnancies, family = binomial, data = d)
summary(model)


intercept <- coef(model)[1]  
coefficient_SevenOrMorePregnancies <- coef(model)[2]
logistic_function <- function(x) {
  1 / (1 + exp(-(intercept + coefficient_SevenOrMorePregnancies * x)))
}
# Calculate probabilities for 7 or more pregnancies (1)
prob_seven_or_more <- logistic_function(1) # 0.5670732 
# Calculate probabilities for six or fewer pregnancies (0)
prob_six_or_fewer<- logistic_function(0) # 0.2849829 


print(prob_six_or_fewer) # 0.2849829 
print(prob_seven_or_more) # 0.5670732 



#checking for multicolinearity
vif_results <- vif(glm(Outcome ~ SkinThickness + BMI  + Glucose + Insulin + DiabetesPedigree + Pregnancies + Age + BloodPressure, family = binomial, data = imputed_data))

# Display the VIF values - no strong multicollinearity, all under 2
print(vif_results)

#Task 4
#Split into training and test
set.seed(91)  # for reproducibility
train_indices <- sample(seq_len(nrow(imputed_data)), size = 0.75 * nrow(imputed_data))
train_data <- imputed_data[train_indices, ]
test_data <- imputed_data[-train_indices, ]

# Identify the column indices or names in 'train_data' corresponding to predictors
predictor_columns <- setdiff(names(train_data), "Outcome")
outcome_col_index <- which(names(train_data) == "Outcome")

# Convert 'train_data' to matrix for training
x_train <- as.matrix(train_data[, predictor_columns, drop = FALSE])
y_train <- train_data$Outcome

# Fit Logistic Regression
logistic_model <- glm(Outcome ~ Pregnancies + Glucose + Glucose*Insulin+ SkinThickness + BMI*SkinThickness +BMI +Age*Pregnancies + DiabetesPedigree, data = train_data, family = binomial)

# Print the logistic regression summary
summary(logistic_model)

x_test <- as.matrix(test_data[, predictor_columns, drop = FALSE])

# Predict using the logistic regression model
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")

# Create confusion matrix
conf_matrix <- confusionMatrix(factor(binary_predictions), factor(test_data$Outcome))
print(conf_matrix)

precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print F1 Score
cat("F1 Score:", f1_score, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

x_to_predict <- as.matrix(ToPredict[, predictor_columns, drop = FALSE])

# Predict using the logistic regression model
logistic_predictions_to_predict <- predict(logistic_model, newdata = ToPredict, type = "response")

# Convert predicted probabilities to binary predictions
binary_predictions_to_predict <- ifelse(logistic_predictions_to_predict > 0.5, 1, 0)

# Display the predictions for 'ToPredict'
result <- data.frame(ToPredict, PredictedOutcome = binary_predictions_to_predict)
print(result)

resultprob <- data.frame(ToPredict, PredictedOutcome = logistic_predictions_to_predict)
print(resultprob)


#Split into training and test and set seed (reproducibility)
set.seed(91)  
train_indices1 <- sample(seq_len(nrow(imputed_data)), size = 0.67 * nrow(imputed_data))
train_data1 <- imputed_data[train_indices, ]
test_data1 <- imputed_data[-train_indices, ]

# Identify the column indices or names in 'train_data' corresponding to predictors
predictor_columns <- setdiff(names(train_data), "Outcome")
outcome_col_index <- which(names(train_data) == "Outcome")

# Convert 'train_data' to matrix for training
x_train <- as.matrix(train_data[, predictor_columns, drop = FALSE])
y_train <- train_data$Outcome

# Fit Logistic Regression
logistic_model <- glm(Outcome ~ BMI +Age +Pregnancies+Glucose, data = train_data, family = binomial)
summary(logistic_model)
# Set up k-fold cross-validation
num_folds <- 10
folds <- createFolds(imputed_data$Outcome, k = num_folds, list = TRUE, returnTrain = FALSE)
accuracy_values <- precision_values <- recall_values <- f1_score_values <- numeric(length = num_folds)
# Initialize vectors to store evaluation metrics for each fold
precision_values <- recall_values <- f1_score_values <- numeric(length = num_folds)

# Perform k-fold cross-validation
for (fold in 1:num_folds) {
  # Split the data into training and testing sets for the current fold
  train_data <- imputed_data[-folds[[fold]], ]
  test_data <- imputed_data[folds[[fold]], ]
  
  # Train the logistic regression model on the training data
  logistic_model_fold <- glm(Outcome ~ BMI +Age +Pregnancies+Glucose, data = train_data, family = binomial)
  
  # Make predictions on the test data
  logistic_predictions <- predict(logistic_model_fold, newdata = test_data, type = "response")
  
  # Convert predicted probabilities to binary predictions
  binary_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)
  
  # Create confusion matrix
  conf_matrix <- confusionMatrix(factor(binary_predictions), factor(test_data$Outcome))
  
  # Calculate Precision, Recall, and F1 Score for the current fold
  accuracy_values[fold] <- conf_matrix$overall["Accuracy"]
  precision_values[fold] <- conf_matrix$byClass["Pos Pred Value"]
  recall_values[fold] <- conf_matrix$byClass["Sensitivity"]
  f1_score_values[fold] <- 2 * (precision_values[fold] * recall_values[fold]) / 
    (precision_values[fold] + recall_values[fold])
  
}

# Calculate the average Precision, Recall, and F1 Score across all folds
average_precision <- mean(precision_values)
average_recall <- mean(recall_values)
average_f1_score <- mean(f1_score_values)
average_accuracy <- mean(accuracy_values)
# Print the average Precision, Recall, and F1 Score
cat("Average Accuracy:", average_accuracy, "\n")
cat("Average Precision:", average_precision, "\n")
cat("Average Recall:", average_recall, "\n")
cat("Average F1 Score:", average_f1_score, "\n")


#Now going to predict the second dataset 
predictor_columns_to_predict <- setdiff(names(ToPredict), "Outcome")

# Convert 'ToPredict' to matrix for prediction
x_to_predict <- as.matrix(ToPredict[, predictor_columns_to_predict, drop = FALSE])

# Predict using the logistic regression model
logistic_predictions_to_predict <- predict(logistic_model, newdata = ToPredict, type = "response")

# Convert predicted probabilities to binary predictions
binary_predictions_to_predict <- ifelse(logistic_predictions_to_predict > 0.5, 1, 0)

