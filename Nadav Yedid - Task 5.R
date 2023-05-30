path = choose.files()
data = read.csv(path)

str(data)
names(data)

#***********************************************************************

# Checking for NA values:
any(is.na(data))
sum(is.na(data$age))
# There are 680 Na values in the age column.

# Removal of NA values in the age column:
titanic_df = na.omit(data)
sum(is.na(titanic_df))
rm(data)
# There are no Na values in the data.

#***********************************************************************

# A model that calculates the chance of survival according to the passenger's age:
model1 = glm(formula = survived == 1 ~ age, family = binomial, data = titanic_df)

# A model that calculates the chance of survival according to the age and gender
# of the passenger:
model2 = glm(formula = survived == 1 ~ age + sex, family = binomial, data = titanic_df)

summary(model1)
summary(model2)

B0_1 = as.numeric(model1$coefficients[1]) # Intercept
B1_1 = as.numeric(model1$coefficients[2]) # age

B0_2 = as.numeric(model2$coefficients[1]) # Intercept
B1_2 = as.numeric(model2$coefficients[2]) # age
B2_2 = as.numeric(model2$coefficients[3]) # male


#***********************************************************************
#***********************************************************************
## Sensitivity (true positive rate) is the probability of a positive test result,
## conditioned on the individual truly being positive.

## Specificity (true negative rate) is the probability of a negative test result,
## conditioned on the individual truly being negative.

## True positive: Sick people correctly identified as sick.
## False positive: Healthy people incorrectly identified as sick.
## True negative: Healthy people correctly identified as healthy.
## False negative: Sick people incorrectly identified as healthy.

## sensitivity, recall, hit rate, or true positive rate (TPR) --> TP/(TP+FN)

## specificity, selectivity or true negative rate (TNR) --> TN/(TN+FP)

## accuracy (ACC) --> (TP+TN)/(P+N) = (TP+TN)/(TP+TN+FP+FN)
#***********************************************************************
#***********************************************************************


# Q1 ----
# Model 1:
# TRUE POSITIVE:
TP1 = sum(sum(predict(model1, type = "response") > 0.5 & titanic_df$survived == 1)) # 33

# FALSE POSITIVE:
FP1 = sum(sum(predict(model1, type = "response") > 0.5 & titanic_df$survived == 0)) # 14

# TRUE NEGATIVE:
TN1 = sum(sum(predict(model1, type = "response") < 0.5 & titanic_df$survived == 0)) # 338

# FALSE NEGATIVE:
FN1 = sum(sum(predict(model1, type = "response") < 0.5 & titanic_df$survived == 1)) # 248

cat("True-Positive, False-Positive, True-Negative & False-Negative values for model 1:
 True positive:", TP1,
    "\n False positive:", FP1,
    "\n True negative:", TN1,
    "\n False negative:", FN1)


# Model 2:
# TRUE POSITIVE:
TP2 = sum(sum(predict(model2, type = "response") > 0.5 & titanic_df$survived == 1)) # 199

# FALSE POSITIVE:
FP2 = sum(sum(predict(model2, type = "response") > 0.5 & titanic_df$survived == 0)) # 44

# TRUE NEGATIVE:
TN2 = sum(sum(predict(model2, type = "response") < 0.5 & titanic_df$survived == 0)) # 308

# FALSE NEGATIVE:
FN2 = sum(sum(predict(model2, type = "response") < 0.5 & titanic_df$survived == 1)) # 82

cat("True-Positive, False-Positive, True-Pegative & False-Pegative values for model 2:
 True positive:", TP2,
    "\n False positive:", FP2,
    "\n True negative:", TN2,
    "\n False negative:", FN2)


aic_model1 = model1$aic  # 869.4682
aic_model2 = model2$aic  # 633.4465 (Better model)

loglike_model1 = logLik(model1)  # -432.7341
loglike_model2 = logLik(model2) # -313.7233 (Better model)

cat("Comparison between the quality of the models according to AIC and LogLik:
 AIC of model 1:", aic_model1,
    "\n AIC of model 2:", aic_model2,
    "\n logLik for model 1:", loglike_model1,
    "\n logLik for model 2:", loglike_model2,
    "\nIt can be seen that in both comparisons Model 2 is more accurate.")


# Q2 ----
# Model 1:

# Sensitivity:
sensitivity1 = TP1/(TP1 + FN1) # 0.1174377

# Specificity:
specificity1 = TN1/(TN1 + FP1) # 0.9602273

# Accuracy:
accuracy1 = (TP1 + TN1)/(TP1 + FP1 + TN1 + FN1) # 0.5860979

cat("Accuracy values for model 1:
 Sensitivity:", sensitivity1,
    "\n Specificity:", specificity1,
    "\n Accuracy:", accuracy1)


# Model 2:

# Sensitivity:
sensitivity2 = TP2/(TP2 + FN2) # 0.7081851

# Specificity:
specificity2 = TN2/(TN2 + FP2) # 0.875

# Accuracy:
accuracy2 = (TP2 + TN2)/(TP2 + FP2 + TN2 + FN2) # 0.8009479

cat("Accuracy values for model 2:
 Sensitivity:", sensitivity2,
    "\n Specificity:", specificity2,
    "\n Accuracy:", accuracy2)

cat("The Accuracy value for both models:
 Model 1:", accuracy1,
    "\n Model 2:", accuracy2,
    "\nOnce again we can see that model 2 is more accurate.")
