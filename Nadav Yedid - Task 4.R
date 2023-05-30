path = choose.files()
data = read.csv(path)
str(data)
names(data)

#***********************************************************************


# Q1 ----
# Checking for NA values:
any(is.na(data))
sum(is.na(data$age))
sum(is.na(data$embarked))
# There are 680 Na values in the age column.

# Removal of NA values in the age column:
titanic_df = na.omit(data)
sum(is.na(titanic_df))
rm(data)
# There are no Na values in the data.

# Removal of (" ") values in the embarked column:
titanic_df = subset(titanic_df, embarked != "")
unique(titanic_df$embarked)


# Q2 ----
str(titanic_df$embarked)

# Changing the categorical variable embarked to a factor (Casting):
titanic_df$embarked = as.factor(titanic_df$embarked)
str(titanic_df$embarked)

# Creating a model for predicting the chance of survival
# depending on age and port of departure:
model_Q2 = glm(formula = survived == 1 ~ embarked + age, family = binomial,
               data = titanic_df)

summary(model_Q2)

B0_1 = as.numeric(model_Q2$coefficients[1]) # Intercept
B1_1 = as.numeric(model_Q2$coefficients[2]) # Queenstown
B2_1 = as.numeric(model_Q2$coefficients[3]) # Southampton
B3_1 = as.numeric(model_Q2$coefficients[4]) # age

### The prediction probability formula to survive:
### P_Y = 1/(1 + exp(-(B0_1 + B1_1*Queenstown + B2_1*Southampton + B3_1*age)))

### Logit formula to survive:
### log(P_Y / (1 - P_Y))


# Q3 ----
## The "Cherbourg" category is the first category.
## Therefore we will change the order of the categories:
titanic_df$embarked = relevel(x = titanic_df$embarked, ref = "Queenstown")

# Creating a model to predict the chance of 30 year old to drown:
model_Q3 = glm(formula = survived == 0 ~ embarked + age, family = binomial,
               data = titanic_df)

summary(model_Q3)

B0_2 = as.numeric(model_Q3$coefficients[1]) # Intercept
B1_2 = as.numeric(model_Q3$coefficients[2]) # Cherbourg
B2_2 = as.numeric(model_Q3$coefficients[3]) # Southampton
B3_2 = as.numeric(model_Q3$coefficients[4]) # age

# Calculation of the chance of drowning by predict:
pred_Cherbourg = predict(model_Q3, data.frame(embarked = "Cherbourg", age = c(30)),
                         type = "response")

# Calculating the chance of drowning using the probability formula:
p_Y_Cherbourg = 1/(1 + exp(-(B0_2 + B1_2*1 + B2_2*0 + B3_2*30)))

cat("The chance of drowning of 30-year-old passengers who sailed from Cherbourg port is:\n Prediction:",
    pred_Cherbourg, "\n Probability formula:",
    p_Y_Cherbourg, "\nIt can be seen that the results are the same.")

# Calculating the chance of drowning using the logit formula:
logit_Cherbourg = predict(model_Q3, data.frame(embarked = "Cherbourg", age = c(30)))

logit_formula = log(p_Y_Cherbourg / (1 - p_Y_Cherbourg))

cat("The chance of drowning of 30-year-old passengers who sailed from Cherbourg port is:\n Logit prediction:",
    logit_Cherbourg, "\n Logit formula:",
    logit_formula, "\nIt can be seen that the results are the same.")


# Q4 ----
model_Q4 = glm(formula = survived == 1 ~ age, family = binomial, data = titanic_df)

summary(model_Q4)

aic_model3 = model_Q3$aic  # 840.21 (Better model)
aic_model4 = model_Q4$aic  # 859.5817

loglike_model3 = logLik(model_Q3)  # -416.105 (Better model)
loglike_model4 =logLik(model_Q4) # -427.7909

cat("Comparison between the quality of the models according to AIC and LogLik:
 AIC of model_Q3:", aic_model3, "\n AIC of model_Q4:", aic_model4,
    "\n logLik for model_Q3:", loglike_model3, "\n logLik for model_Q4:", loglike_model4,
    "\nIt can be seen that in both comparisons Model 3 is more accurate.")


# Q5 ----
max(titanic_df$age)
# The oldest person was 71 years old.

# Converting the ages into categories:
titanic_df$age_category = cut(titanic_df$age,
                              breaks = c(0, 9, 19, 29, 39, 49, 59, 69, Inf),
                              labels = c("0-9", "10-19", "20-29", "30-39", "40-49",
                                         "50-59", "60-69", "70+"))

class(titanic_df$age_category)
# The class of the new column is "factor".

tapply(X = titanic_df$survived == 1, INDEX = titanic_df$age_category, FUN = mean)
tapply(X = titanic_df$survived == 1, INDEX = titanic_df$age_category, FUN = sum)
# It can be seen that on average the children between the ages of 0-9
# have the best chance of survival - 71.73%.

# In addition, it can be seen that the largest number of people who survived
# were in the age range of 20-29 - 67.

# ----
#### Converting the ages into categories as we learned in class:
#### titanic_df$age = floor(titanic_df$age / 10)
#### titanic_df$age = as.factor(titanic_df$age)

#### tapply(X = titanic_df$survived, INDEX = titanic_df$age, FUN = mean)
#### tapply(X = titanic_df$survived, INDEX = titanic_df$age, FUN = sum)
# ----

# Creating a logistic regression model:
# The chance of survival by age of 0-9:
model_Q5 = glm(formula = survived ~ age_category == "0-9", family = binomial,
             data = titanic_df)

summary(model_Q5)

temp_B0_1 = as.numeric(model_Q5$coefficients[2])
OddsRatio1 = exp(temp_B0_1)
cat("The odds ratio of passengers in the 0-9 age category is:", OddsRatio1)
# ----


# Q6 ----
# The odds ratio between those who wear a helmet and those who don't is 0.3.
# Since the result is less than 1, we understand that the odds of those
# who do not wear a helmet are greater.
## Therefore answer D is correct.




#*************************************************************
#*************************************************************
#### In case there is more than 1 category: ####
## The 0-9 category is the first category.
## Therefore we will change the order of the categories:
titanic_df$age_category = relevel(x = titanic_df$age_category, ref = "20-29")

model_Q6 = glm(formula = survived ~ age_category, family = binomial,
             data = titanic_df)

summary(model_Q6)

temp_B0_2 = as.numeric(model_Q6$coefficients["age_category0-9"])
OddsRatio2 = exp(temp_B0_2)
cat("The odds ratio of passengers in the 0-9 age category is:", OddsRatio2)
