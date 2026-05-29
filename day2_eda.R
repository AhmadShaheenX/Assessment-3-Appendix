library(tidyverse)
library(skimr)
library(ggcorrplot)
library(patchwork)

df <- read_csv("diabetes_3class_clean.csv")

#Step 1: Quick descriptive stats summary of all 31 features(variable) 
skim(df)

#Step 2: Examine diabetes class distribution (bar chart > run to find out)
ggplot(df, aes(x = diabetes_class)) + 
  geom_bar() + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) + 
  labs(title = "Diabetes Class Distribution", x = "Diabetes Classes", y = "Count")

#Step 3: Feature (variable) distribution (histogram/Bar chart > run to find out), (Leakage Columns Dropped/Not Included)
##Clinical/Physiological Features
###BMI (Hist)
ggplot(df, aes(x = bmi, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "BMI Distribution", x = "BMI Range", y = "Count")

###Waist_to_hip_ratio (Hist)
ggplot(df, aes(x = waist_to_hip_ratio, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Waist_to_Hip_Ratio Distribution", x = "Waist_to_Hip_Ratio Range", y = "Count")

###Systolic_bp (Hist)
ggplot(df, aes(x = systolic_bp, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Systolic_BP Distribution", x = "Sys_BP Range", y = "Count")

###Diastolic_bp (Hist)
ggplot(df, aes(x = diastolic_bp, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Diastolic_BP Distribution", x = "Dia_BP Range", y = "Count")

###Heart_Rate (Hist)
ggplot(df, aes(x = heart_rate, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Heart_Rate Distribution", x = "Heart_Rate Range", y = "Count")

###Cholesterol total (Hist)
ggplot(df, aes(x = cholesterol_total, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Total Cholesterol Distribution", x = "Total Cholesterol Range", y = "Count")

###Hdl_Cholesterol (Hist)
ggplot(df, aes(x = hdl_cholesterol, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "HDL Cholesterol Distribution", x = "HDL Cholesterol Range", y = "Count")

###Ldl_Cholesterol (Hist)
ggplot(df, aes(x = ldl_cholesterol, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "LDL Cholesterol Distribution", x = "LDL Cholesterol Range", y = "Count")

###Triglycerides (Hist)
ggplot(df, aes(x = triglycerides, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Triglycerides Distribution", x = "Triglycerides Range", y = "Count")

###Family_History_Diabetes (Bar)
ggplot(df, aes(x = factor(family_history_diabetes), fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  labs(title = "Family_History_Diabetes Distribution", x = "Family_History_Diabetes", y = "Count")

###Hypertension_History (Bar)
ggplot(df, aes(x = factor(hypertension_history), fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  labs(title = "Hypertension_History Distribution", x = "Hypertension_History", y = "Count")

###Cardiovascular_History (Bar)
ggplot(df, aes(x = factor(cardiovascular_history), fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  labs(title = "Cardiovascular_History Distribution", x = "Cardiovascular_History", y = "Count")

##Socioeconomic/Demographic Features
###Age (Hist)
ggplot(df, aes(x = age, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Age Distribution", x = "Age Range", y = "Count")

###Gender (Bar)
ggplot(df, aes(x = gender, fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")

###Ethnicity (Bar)
ggplot(df, aes(x = ethnicity, fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ diabetes_class) +
  labs(title = "Ethnicity Distribution", x = "Ethnicity", y = "Count")

###Education_Level (Bar)
ggplot(df, aes(x = education_level, fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ diabetes_class) +
  labs(title = "Education_Level Distribution", x = "Education_Level", y = "Count")

###Income_Level (Bar)
ggplot(df, aes(x = income_level, fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ diabetes_class) +
  labs(title = "Income_Level Distribution", x = "Income_Level", y = "Count")

###Employment_Status (Bar)
ggplot(df, aes(x = employment_status, fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  labs(title = "Employment_Status Distribution", x = "Employment_Status", y = "Count")

##Behavioural/Lifestyle
###Smoking_Status (Bar)
ggplot(df, aes(x = smoking_status, fill = diabetes_class)) +
  geom_bar(position = "dodge") +
  labs(title = "Smoking_Status Distribution", x = "Smoking_Status", y = "Count")

###Alcohol_Consumption_Per_Week (Hist)
ggplot(df, aes(x = alcohol_consumption_per_week, fill = diabetes_class)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ diabetes_class) + 
  labs(title = "Alcohol_Consumption_Per_Week Distribution", x = "Alcohol_Consumption_Per_Week", y = "Count")

###Physical_Activity_Minutes_Per_Week (Hist)
ggplot(df, aes(x = physical_activity_minutes_per_week, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Physical_Activity_Minutes_Per_Week Distribution", x = "Physical_Activity_Minutes_Per_Week Range", y = "Count")

###Diet_Score (Hist)
ggplot(df, aes(x = diet_score, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Diet_Score Distribution", x = "Diet_Score Range", y = "Count")

###Sleep_Hours_Per_Day (Hist)
ggplot(df, aes(x = sleep_hours_per_day, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Sleep_Hours_Per_Day Distribution", x = "Sleep_Hours_Per_Day Range", y = "Count")

###Screen_Time_Hours_Per_Day (Hist)
ggplot(df, aes(x = screen_time_hours_per_day, fill = diabetes_class)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ diabetes_class) +
  labs(title = "Screen_Time_Hours_Per_Day Distribution", x = "Screen_Time_Hours_Per_Day Range", y = "Count")

#Step 4: Correlation heat map (only numeric columns, dropped columns excluded)
df %>%
  select(where(is.numeric)) %>%
  select(-c(glucose_fasting, glucose_postprandial, insulin_level, hba1c, diabetes_risk_score, diagnosed_diabetes)) %>%
  cor() %>%
  ggcorrplot(lab = TRUE, lab_size = 2)

#Step 5: Feature-target relationships (as box plots)
##Do whichever ones are most interesting/relevant according to what was found in step3 earlier, this is only an example
p1 <- ggplot(df, aes(x = diabetes_class, y = bmi, fill = diabetes_class)) +
  geom_boxplot() +
  labs(title = "BMI", x = "Diabetes_Class", y = "BMI")

p2 <- ggplot(df, aes(x = diabetes_class, y = systolic_bp, fill = diabetes_class)) +
  geom_boxplot() +
  labs(title = "Systolic_BP", x = "Diabetes_Class", y = "Sys_BP")

p3 <- ggplot(df, aes(x = diabetes_class, y = age, fill = diabetes_class)) +
  geom_boxplot() +
  labs(title = "Age", x = "Diabetes_Class", y = "Age")

p4 <- ggplot(df, aes(x = diabetes_class, y = waist_to_hip_ratio, fill = diabetes_class)) +
  geom_boxplot() +
  labs(title = "Waist_to_hip_ratio", x = "Diabetes_Class", y = "Waist_to_hip_ratio")

p5 <- ggplot(df, aes(x = diabetes_class, y = physical_activity_minutes_per_week, fill = diabetes_class)) +
  geom_boxplot() +
  labs(title = "Physical_activity_minutes_per_week", x = "Diabetes_Class", y = "Physical_activity_minutes_per_week")

p1 + p2 + p3 + p4 + p5

#Step 6: Subgroup distribution analysis (for socioeconomic/demographic features except age)
##Ethnicity
ggplot(df, aes(x = ethnicity, fill = diabetes_class)) +
  geom_bar(position = "fill") +
  labs(title = "Ethnicity Distribution", x = "Ethnicity", y = "Proportion")

##Gender
ggplot(df, aes(x = gender, fill = diabetes_class)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Distribution", x = "Gender", y = "Proportion")

##Education_Level
ggplot(df, aes(x = education_level, fill = diabetes_class)) +
  geom_bar(position = "fill") +
  labs(title = "Education_Level Distribution", x = "Education_Level", y = "Proportion")

##Income_Level
ggplot(df, aes(x = income_level, fill = diabetes_class)) +
  geom_bar(position = "fill") +
  labs(title = "Income_Level Distribution", x = "Income_Level", y = "Proportion")

##Employment_Status
ggplot(df, aes(x = employment_status, fill = diabetes_class)) +
  geom_bar(position = "fill") +
  labs(title = "Employment_Status Distribution", x = "Employment_Status", y = "Proportion")
