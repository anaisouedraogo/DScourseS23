library(sampleSelection)
library(tidyverse)
library(modelsummary)

# Set the working directory to the folder where the "wages.csv" file is located
setwd("C:/DScourseS23/ProblemSets/PS12")

# Load the "wages.csv" file as a data frame
wages_df <- read.csv(file="wages12.csv", sep = ",", header=T)

#convert to factor
wages_df$college <- factor(wages_df$college)
wages_df$married <- factor(wages_df$married)
wages_df$union <- factor(wages_df$union)

# Produce a summary table of the model
datasummary_skim(wages_df,histogram=F,output="latex")

# Perform listwise deletion to remove any row where "logwage" is missing
df_wages1 <- wages_df[complete.cases(wages_df$logwage),]
# Estimate a simple linear regression model
model1 <- lm(logwage ~ hgc + union +college+ exper + I(exper^2), data = df_wages1)
summary_listwise <- modelsummary(model1)
print(summary_listwise)

#mean imputation

# Compute the mean of non-missing logwage values
df_wages2<-wages_df
mean_logwage <- mean(wages_df$logwage, na.rm = TRUE)

# Replace missing logwage values with the mean
mean_logwage<- df_wages2$logwage[is.na(df_wages2$logwage)] 
# Estimate a simple linear regression model
model2 <- lm(logwage ~ hgc + union +college+ exper + I(exper^2), data = df_wages2)
summary_mean_imputation <- modelsummary(model2)
print(summary_mean_imputation)

# Create a new variable called valid which flags log wage observations that are not missing
wages_df$valid<- !is.na(wages_df$logwage)

# Recode the log wage variable so that invalid observations are now equal to 0
wages_df$logwage[!wages_df$valid] <- 0

# Estimate the Heckman selection 
model3<-selection(selection = valid ~ hgc + union + college + exper + married + kids,
          outcome = logwage ~ hgc + union + college + exper + I(exper^2),
          data = wages_df, method = "2step")
modelsummary(list(model1,model2,model3),output = "latex")

#probit model
probit_model <-glm(union ~ hgc + college + exper + married + kids,
                    data = wages_df, family = binomial(link = "probit"))
print(summary(probit_model))

#prediction
wages_df %<>% mutate(predProbit = predict(probit_model, newdata = wages_df, type = "response"))
wages_df %>% `$`(predProbit) %>% summary %>% print

# counterfactual policy
probit_model$coefficients["married"] <- 0
probit_model$coefficients["kids"] <- 0
wages_df %<>% mutate(predLogitCfl = predict(probit_model, newdata = wages_df, type = "response"))
wages_df %>% `$`(predLogitCfl) %>% summary %>% print
