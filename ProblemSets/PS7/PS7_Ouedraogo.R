library(mice)
library(modelsummary)

# Set the working directory to the folder where the "wages.csv" file is located
setwd("C:/DScourseS23/ProblemSets/PS7")

# Load the "wages.csv" file as a data frame
wages_df <- read.csv(file="wages.csv", sep = ",", header=T)

# Drop observations where either "hgc" or "tenure" are missing
df_wages <- wages_df[complete.cases(wages_df[c("hgc", "tenure")]),]


# Estimate a  linear regression model
model <- lm(logwage ~ hgc + tenure + I(tenure^2) + age + married + college, data = df_wages)

# Produce a summary table of the model
model_summary <- modelsummary(model)
print(model_summary)



# Perform listwise deletion to remove any row where "logwage" is missing
df_wages1 <- df_wages[complete.cases(df_wages$logwage),]
# Estimate a simple linear regression model
model1 <- lm(logwage ~ hgc + tenure + I(tenure^2) + age + married + college, data = df_wages1)
summary_listwise <- modelsummary(model1)
print(summary_listwise)

#mean imputation

# Compute the mean of non-missing logwage values
df_wages2<-df_wages
mean_logwage <- mean(df_wages$logwage, na.rm = TRUE)

# Replace missing logwage values with the mean
mean_logwage<- df_wages2$logwage[is.na(df_wages2$logwage)] 
# Estimate a simple linear regression model
model2 <- lm(logwage ~ hgc + tenure + I(tenure^2) + age + married + college, data = df_wages2)
summary_mean_imputation <- modelsummary(model2)
print(summary_mean_imputation)


#predicted value imputation

predicted_logwage <- predict(model1, newdata = df_wages1)
df_wages3<- df_wages
df_wages$logwage[is.na(df_wages$logwage)] <- predicted_logwage
model3 <- lm(logwage ~ hgc + tenure + I(tenure^2) + age + married + college, data = df_wages3)
summary_predicted_imputation <- modelsummary(model3)
print(summary_predicted_imputation)



# Copy original data frame
df_wages4 <- df_wages

# Set seed for reproducibility
set.seed(123)

# Create mice object
imp <- mice(df_wages4, method = "pmm", m = 5)

# Complete the imputation process
imputed_data <- complete(imp)

# Update imputed values in data frame
df_wages4$logwage <- imputed_data$logwage
# Fit linear regression model to imputed data
model4 <- lm(logwage ~ hgc + tenure + I(tenure^2) + age + married + college, data = df_wages4)
summary_multiple_imputation <- modelsummary(model4)
print(summary_multiple_imputation)


# create a list of models
model_list <- list(
  "Complete Cases" = model1,
  "Mean Imputation" = model2,
  "Regression Imputation" = model3,
  "Multiple Imputation" = model4
)

# generate summary table
modelsummary(model_list, output = "latex")
