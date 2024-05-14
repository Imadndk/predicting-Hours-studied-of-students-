student_data<-read.csv("C:/Users/Hp/Desktop/Student_Performance.csv")
head(student_data)
student_data$Extracurricular.Activities <- as.factor(ifelse(student_data$Extracurricular.Activities=="Yes", 1, 0))
head(student_data)
colSums(is.na(student_data))

options(repr.plot.width =4, repr.plot.height = 4)
boxplot(student_data$Sleep.Hours)
boxplot(student_data$Previous.Scores)
boxplot(student_data$Performance.Index)
boxplot(student_data$Sample.Question.Papers.Practiced)


install.packages("ggplot2")
library(ggplot2)
ggplot(student_data, aes(x = `Hours.Studied`)) +  # Replace 'Sleep Hours' with actual column name
  geom_histogram(bins = 7, aes(fill = ..density..), color = "black") +
  geom_density(alpha = 0.09) +  # Add KDE line with 0.9 transparency
  labs(title = "Distribution Plot of Hours Studied of Students", x = "Hours Studied") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))  # Center title

install.packages("ggcorrplot")
library(ggcorrplot)
options(repr.plot.width =6, repr.plot.height = 4)
student_cor <- round(cor(student_data[,c("Hours.Studied", "Performance.Index", "Sleep.Hours","Previous.Scores")]), 1)
ggcorrplot(student_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))
library(caret)
X <- student_data[, !names(student_data) %in% "Hours.Studied"]
Y <- student_data$Hours.Studied
# Split the data into training and testing sets
set.seed(122) # Set seed for reproducibility
train_index <- createDataPartition(Y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
Y_train <- Y[train_index]
X_test <- X[-train_index, ]
Y_test <- Y[-train_index]
model <-glm(Hours.Studied ~ Performance.Index + Sleep.Hours+Sample.Question.Papers.Practiced+Extracurricular.Activities+Previous.Scores, family = poisson(link = "log"), data = student_data[train_index,])
summary(model)

# Predict using new data (replace telecom_final with your actual data)
predictions <- predict(model, newdata = student_data[-train_index, ], type = "response")

# Poisson regression predicts the expected count, not a specific count
print(paste("Predicted expected counts:", predictions[1:5]))  # Show first 5 predictions

# You can interpret the predictions as the average number of events expected to occur
# based on the new data point and the model fit.

# Note: Predicted values might not be whole numbers due to the nature of count data.

# Example MSE calculation (replace with your actual data)
mse <- mean((predictions - Y_test)^2)
print(paste("Mean Squared Error:", mse))

# Calculate deviance (replace with your actual data)
null_deviance <- deviance(glm(Hours.Studied ~ 1, family = poisson, data = student_data))
model_deviance <- deviance(model)
deviance_explained <- (null_deviance - model_deviance) / null_deviance
print(paste("Deviance Explained:", deviance_explained))

install.packages("pROC")

y_pred_proba <- predict(model, newdata = X_test, type = "response")

# Calculate the ROC curve
library(pROC)
roc_obj <- roc(Y_test, y_pred_proba)

# Print AUC
print(paste("AUC:", auc(roc_obj)))

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "blue", lwd = 2, print.thres = TRUE)
