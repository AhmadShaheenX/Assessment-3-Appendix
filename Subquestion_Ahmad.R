library(caret)
library(randomForest)
library(kernlab)
library(themis)
library(MLmetrics)

df_ml <- df %>%
  dplyr::select(
    diabetes_class,
    smoking_status,
    alcohol_consumption_per_week,
    physical_activity_minutes_per_week,
    diet_score,
    sleep_hours_per_day,
    screen_time_hours_per_day
  ) %>%
  mutate(across(where(is.character), as.factor),
         diabetes_class = as.factor(make.names(diabetes_class)))

set.seed(123)
sample_index <- createDataPartition(df_ml$diabetes_class, p = 0.1, list = FALSE)
df_sample <- df_ml[sample_index, ]

rfe_ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
x_vars <- df_sample %>% dplyr::select(-diabetes_class)
y_var <- df_sample$diabetes_class

rfe_results <- rfe(x_vars, y_var, sizes = c(1:6), rfeControl = rfe_ctrl)
plot(rfe_results, type = c("g", "o"), main = "RFE: Feature Performance Curve")

set.seed(42)
train_idx <- createDataPartition(df_sample$diabetes_class, p = 0.7, list = FALSE)
train_data <- df_sample[train_idx, ]
test_data <- df_sample[-train_idx, ]

cv_ctrl <- trainControl(
  method = "cv",
  number = 10,
  sampling = "smote",
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

model_lr <- train(diabetes_class ~ ., data = train_data, method = "multinom", trControl = cv_ctrl, trace = FALSE)
model_rf <- train(diabetes_class ~ ., data = train_data, method = "rf", trControl = cv_ctrl, tuneLength = 3)
model_svm <- train(diabetes_class ~ ., data = train_data, method = "svmRadial", trControl = cv_ctrl, tuneLength = 3)

resamps <- resamples(list(LogReg = model_lr, RandForest = model_rf, SVM_RBF = model_svm))
bwplot(resamps, main = "Model Comparison: Performance Metrics")

pred_rf <- predict(model_rf, test_data)
cm_rf <- confusionMatrix(pred_rf, test_data$diabetes_class)
print(cm_rf)

rf_importance <- varImp(model_rf, scale = TRUE)
plot(rf_importance, main = "Variable Importance: Lifestyle Factors")

ggplot(train_data, aes(x = diabetes_class, fill = diabetes_class)) +
  geom_bar(color = "black", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Class Distribution in Training Data",
    x = "Diabetes Class",
    y = "Count"
  ) +
  theme(legend.position = "none")

risk_trends <- data.frame(
  Physical_Activity = test_data$physical_activity_minutes_per_week,
  Diet = test_data$diet_score,
  Type2_Risk_Probability = predict(model_rf, test_data, type = "prob")$Type.2
)

ggplot(risk_trends, aes(x = Physical_Activity, y = Type2_Risk_Probability)) +
  geom_smooth(color = "darkred", fill = "pink", method = "loess") +
  theme_minimal() +
  labs(
    title = "Risk Curve: Physical Activity vs. Type 2 Diabetes Risk",
    x = "Physical Activity (Minutes per Week)",
    y = "Predicted Probability of Type 2 Diabetes"
  )

ggplot(risk_trends, aes(x = Diet, y = Type2_Risk_Probability)) +
  geom_smooth(color = "darkblue", fill = "lightblue", method = "loess") +
  theme_minimal() +
  labs(
    title = "Risk Curve: Diet Score vs. Type 2 Diabetes Risk",
    x = "Diet Score",
    y = "Predicted Probability of Type 2 Diabetes"
  )

lr_importance <- varImp(model_lr, scale = TRUE)
plot(lr_importance, main = "Variable Importance: Logistic Regression")

library(pROC)

rf_probs <- predict(model_rf, test_data, type = "prob")

roc_no <- roc(test_data$diabetes_class == "No.Diabetes", rf_probs$No.Diabetes, quiet = TRUE)
roc_pre <- roc(test_data$diabetes_class == "Pre.Diabetes", rf_probs$Pre.Diabetes, quiet = TRUE)
roc_t2 <- roc(test_data$diabetes_class == "Type.2", rf_probs$Type.2, quiet = TRUE)

plot(roc_no, col = "#1f77b4", main = "Multi-class ROC: Random Forest")
plot(roc_pre, add = TRUE, col = "#ff7f0e")
plot(roc_t2, add = TRUE, col = "#2ca02c")
legend("bottomright", 
       legend = c(paste0("Type 2 AUC: ", round(auc(roc_t2), 3)), 
                  paste0("Pre-Diab AUC: ", round(auc(roc_pre), 3)),
                  paste0("No Diab AUC: ", round(auc(roc_no), 3))),
       col = c("#2ca02c", "#ff7f0e", "#1f77b4"), lwd = 4)
