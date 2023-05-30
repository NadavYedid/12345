# Nadav Yedid 208271007

# Q1 ----
path = choose.files()
DF = read.csv(path)

str(DF)
names(DF)

# Checking for NA values:
any(is.na(DF))
# There are no Na values in the data.

head(DF, 10)


# Q2.1 ----
hist(DF$AccidentsYear, main = "Histogram for Accidents per year",
     xlab = "Number of Accidents", col.main = "blue", font.main = 4,
     cex.lab = 1.2, cex.main = 1.3)

hist(DF$Workers, main = "Histogram for Workers",
     xlab = "Number of Workers", col.main = "blue", font.main = 4,
     cex.lab = 1.2, cex.main = 1.3)

# Q2.2 ----
accidents_per_month = mean(DF$AccidentsYear) / 12 #  0.07166667

cat("The rate of accidents per month is:", accidents_per_month)


# Q3 ----
plot(DF$AccidentsYear ~ DF$Workers, pch = 11, col = "blue",
     main = "The connection between the number of accidents
     and the number of workers",
     xlab = "Workers",
     ylab = "Accidents",
     col.main = "blue", font.main = 4, cex.lab = 1.2, cex.main = 1.3)


# Q4 ----
modelHW1 = glm(AccidentsYear ~ Workers, data = DF, family = poisson())
modelHW2 = glm(AccidentsYear ~ sqrt(Workers), data = DF, family = poisson())
modelHW3 = glm(AccidentsYear ~ log(Workers), data = DF, family = poisson())

summary(modelHW1)
summary(modelHW2)
summary(modelHW3)

p_value_1 = as.numeric(summary(modelHW1)$coefficients["Workers", "Pr(>|z|)"])
p_value_2 = as.numeric(summary(modelHW2)$coefficients["sqrt(Workers)", "Pr(>|z|)"])
p_value_3 = as.numeric(summary(modelHW3)$coefficients["log(Workers)", "Pr(>|z|)"])

loglike_model1 = logLik(modelHW1)
loglike_model2 = logLik(modelHW2)
loglike_model3 = logLik(modelHW3)

cat("Comparison between the quality of the models according P-value and LogLik:
 P-value of model 1:", p_value_1,
    "\n P-value of model 2:", p_value_2,
    "\n P-value of model 3:", p_value_3,
    "\n LogLik for model 1:", loglike_model1,
    "\n LogLik for model 2:", loglike_model2,
    "\n LogLik for model 3:", loglike_model3,
    "\n\nIt can be seen that the differences in the P-values are negligible.
In addition, we can see that LogLik of model 3 is the highest (less negative).
Therefore this model is the best fit to the data.
Answer 9 is correct.")


# Q5.1 ----
plot(predict(modelHW3) ~ log(DF$Workers), pch = 11, col = "blue",
     main = "The prediction points along the equation of the straight line",
     xlab = "Workers",
     ylab = "Predict",
     col.main = "blue", font.main = 4, cex.lab = 1.2, cex.main = 1.5)
lines(predict(modelHW3) ~ log(DF$Workers), col = "red", lwd = 1.5)


# Q5.2 ----
plot(DF$AccidentsYear ~ log(DF$Workers), pch = 11, col = "blue",
     main = "The distribution of accidents depending on the LOG of the
     number of employees",
     xlab = "Workers",
     sub="(LOG)",
     ylab = "Accidents",
     col.main = "blue", font.main = 4, cex.main = 1.3, font.sub = 12,
     cex.lab = 1.2, cex.sub = 0.8)
lines(predict(modelHW3) ~ log(DF$Workers), col = "red", lwd = 1.5)


# Q6 ----
B0 = as.numeric(modelHW3$coefficients[1]) # Intercept
B1 = as.numeric(modelHW3$coefficients[2]) # log(Workers)

# In order to find the lambda values, 40 was chosen as the number of workers.
predict_lambda1 = predict(modelHW3, data.frame(Workers = 40),
                        type = "response") # lambda = 2.629215  
manually_lambda1 = exp(B0 + B1*log(40)) # lambda = 2.629215
## This is the correct equation when we write **type = "response"** in the predict.
## type = "response" cancels the LOG.

cat("Predicted lambda:", predict_lambda1,
    "\nManually lambda:", manually_lambda1,
    "\nIt can be seen that the lambda values are the same.")

predict_lamda2 = predict(modelHW3, data.frame(Workers = 40)) # lambda = 0.9666852
manually_lambda2 = B0 + (B1*log(40)) # lambda = 0.9666852
log_lambda = log(manually_lambda1) # lambda = 0.9666852
## This is the correct equation when we **do not write type = "response"**.
## It can be seen that the lambda values are the same.

cat("Predicted lambda:", predict_lamda2,
    "\nManually lambda:", manually_lambda2,
    "\nLog lambda:", log_lambda,
    "\nIt can be seen that the lambda values are the same.")

B0*(40^B1) # -44.82324

### All the answers are correct.


# Q7 ----
# Finding the lambda by using predict function:
predict(modelHW3, data.frame(Workers = 30), type = "response") # 2.070597

# Manually finding lambda:
lambda3 = exp(B0 + B1 * log(30)) # 2.070597


# Q8 ----
cat("The meaning of B1 in model 2 is the rate of change of the number of accidents.")


# Q9 ----
rate_1 = mean(predict(modelHW1, data.frame = DF, type = "response"))
rate_2 = mean(predict(modelHW2, data.frame = DF, type = "response"))
rate_3 = mean(predict(modelHW3, data.frame = DF, type = "response"))
rate_4 = mean(DF$AccidentsYear)

cat("Comparison of accident rates according to models:
 The rate of accidents according to model 1:", rate_1,
    "\n The rate of accidents according to model 2:", rate_2,
    "\n The rate of accidents according to model 3:", rate_3,
    "\n The rate of accidents according to the original data:", rate_4,
    "\nIt can see that all the rates are the same.
Answers 1-4 are correct.")


# Q10 ----
predict_lambda_30 = predict(modelHW3, data.frame(Workers = 30),
                          type = "response") # 2.070597

prob_3 = dpois(x = 3, lambda = predict_lambda_30) # 0.1865893

cat("The chances that a construction site with 30 workers
will have 3 accidents per year is:", prob_3)


# Q11 ----
linear_model = lm(AccidentsYear ~ Workers, data = DF)
poisson_model = glm(AccidentsYear ~ Workers, data = DF, family = poisson())

plot(DF$AccidentsYear ~ DF$Workers, pch = 11, col = "blue",
     main = "Comparison between prediction lines of
linear regression and Poisson regression",
     xlab = "Workers",
     ylab = "Accidents",
     col.main = "blue", font.main = 4, cex.lab = 1.2, cex.main = 1.3)

lines(predict(linear_model, type = "response") ~ DF$Workers, col = "red", lwd = 1.5)
lines(predict(poisson_model, data.frame(Workers = DF[order(DF$Workers), "Workers"]),
              type = "response") ~ sort(DF$Workers), col = "green", lwd = 1.5)
