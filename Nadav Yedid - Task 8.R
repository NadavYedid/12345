library(MASS)
library(AER)

accidents_before <- c(210, 205, 195, 185, 173)
year <- c(5:1)

data <- data.frame(accidents = accidents_before, year = year)
plot(data$accidents ~ data$year, xlim = c(5, 1),
     pch = 1, col = "blue",
     main = "Plot of accidents by years",
     xlab = "Year",
     ylab = "Accidents",
     col.main = "blue", font.main = 4, cex.lab = 1.2, cex.main = 2)


# Q1 ----
# Step 1 - Pi and Lambda:
pi1 <- 173
lambda <- 144


# Step 2 - Variance of Pi and Variance of Lambda:
## var(pi1) = pi1
## var(lambda) = lambda


# Step 3 - Delta and Theta:
delta1 <- pi1 - lambda
# There is a decrease of 29 in the number of accidents.

theta1 <- (lambda / pi1) / (1 + (pi1 / (pi1^2))) # 0.8275862:
# There is a relative decrease of 0.1724138 (1 - 0.8275862) in the number of accidents.


# Step 4 - Variance of Delta and Variance of Theta:
var_delta1 <- pi1 + lambda # 317

var_theta1 <- (theta1^2) * ((pi1 / pi1^2) + (lambda / lambda^2)) /
  (1 + (pi1 / pi1^2))^2 # 0.008615309


#********************************************************
#********************************************************
# Delta (29) is the desired decrease in accidents (improvement in safety),
# due to the intervention.

# Only when Delta is a positive number, there is an improvement in safety
# (a negative Delta is an increase in accidents).

# If the CI (confidence interval) of Delta includes zero,
# the intervention is not effective with 95% confidence.

# This means that only when Delta's CI includes positive numbers
# is the intervention effective with 95% confidence.
#********************************************************
#********************************************************


# Step 5 - Creating a confidence interval for Delta:
delta_upper1 <- delta1 + 1.96 * sqrt(var_delta1)
# Delta upper CI is 63.89681 

delta_lower1 <- delta1 - 1.96 * sqrt(var_delta1)
# Delta lower CI is -5.896808 (minus is increase in accidents).

cat("The Confidence Interval of Delta is between", delta_upper1, "and", delta_lower1,
    "\nSince 0 is within the confidence interval, no significant improvement can be claimed.")


#********************************************************
#********************************************************
# 1 - Theta (1 - 0.8275862 = 0.1724138) is the relative desired decrease in accidents
# (improvement in safety), due to the intervention.

# Only when Theta is a positive fraction, there is a decrease in accidents
# (Theta above 1 is an increase in accidents).

# If the CI (confidence interval) of Theta includes one,
# the intervention is not effective with 95% confidence.

# This means that only when the CI of Theta includes positive fractions below 1,
# the intervention is effective with 95% confidence.
#********************************************************
#********************************************************


# Step 6 - Creating a confidence interval for Theta:
theta_upper1 <- theta1 + 1.96 * sqrt(var_theta1)
# Theta upper CI is 1.009511

theta_lower1 <- theta1 - 1.96 * sqrt(var_theta1)
# Theta lower CI is 0.6456616

cat("The Confidence Interval of Theta is between", theta_upper1, "and", theta_lower1,
    "\nSince 1 is within the confidence interval, no significant improvement can be claimed.")
#_______________________________________________________________________________________________



# Q2 ----
# Step 1 - Pi and Lambda:
pi2 <- mean(tail(data$accidents, 3))
# pi2 is 184.3333.


# Step 2 - Variance of Pi and Variance of Lambda:
## var(pi2) = pi2
## var(lambda) = lambda


# Step 3 - Delta and Theta:
delta2 <- pi2 - lambda
# There is a decrease of 40.33333 in the number of accidents.

theta2 <- (lambda / pi2) / (1 + (pi2 / (pi2^2))) # 0.7769784:
# There is a relative decrease of 0.2230216 (1 - 0.7769784) in the number of accidents.


# Step 4 - Variance of Delta and Variance of Theta:
var_delta2 <- pi2 + lambda # 328.3333

var_theta2 <- (theta2^2) * ((pi2 / pi2^2) + (lambda / lambda^2)) /
  (1 + (pi2 / pi2^2))^2 # 0.007386985


# Step 5 - Creating a confidence interval for Delta:
delta_upper2 <- delta2 + 1.96 * sqrt(var_delta2)
# Delta upper CI is 75.84848.

delta_lower2 <- delta2 - 1.96 * sqrt(var_delta2)
# Delta lower CI is 4.818191.

cat("The Confidence Interval of Delta is between", delta_upper2, "and", delta_lower2,
    "\nSince 0 is not within the confidence interval, a significant improvement can be claimed.",
    "\nThere is a decrease of 40.33333 in accidents (an improvement of safety).")


# Step 6 - Creating a confidence interval for Theta:
theta_upper2 <- theta2 + 1.96 * sqrt(var_theta2)
# Theta upper CI is 0.9454357.

theta_lower2 <- theta2 - 1.96 * sqrt(var_theta2)
# Theta lower CI is 0.6085212.

cat("The Confidence Interval of Theta is between", theta_upper2, "and", theta_lower2,
    "\nSince 1 is not within the confidence interval, a significant improvement can be claimed.",
    "\nThere is a decrease of", (1 - theta2), "(1 - theta2) in accidents (an improvement of safety).")
#_______________________________________________________________________________________________



# Q3 ----
pois_model <- glm(accidents ~ year, data = data, family = poisson())
summary(pois_model)


# Step 1 - Pi and Lambda:
pi3 <- predict(pois_model, newdata = data.frame(year = c(0)), type = "response") 
# pi3 is 166.9381.


# Step 2 - Variance of Pi and Variance of Lambda:
## var(lambda) = lambda

var_pi3 <- as.numeric(predict(pois_model, newdata = data.frame(year = c(0)), type = "response",
                             se = TRUE)[2])^2
# var_p13 = 167.2898 


# Step 3 - Delta and Theta:
delta3 <- pi3 - lambda
# There is a decrease of 22.93807 in the number of accidents.

theta3 <- (lambda / pi3) / (1 + (var_pi3 / (pi3^2))) # 0.8574482:
# There is a relative decrease of 0.1425518 (1 - 0.8574482) in the number of accidents.


# Step 4 - Variance of Delta and Variance of Theta:
var_delta3 <- pi3 + lambda # 310.9381

var_theta3 <- (theta3^2) * ((var_pi3 / pi3^2) + (lambda / lambda^2)) /
  (1 + (var_pi3 / pi3^2))^2 # 0.009405827


# Step 5 - Creating a confidence interval for Delta:
delta_upper3 <- delta3 + 1.96 * sqrt(var_delta3)
# Delta upper CI is 57.4996.

delta_lower3 <- delta3 - 1.96 * sqrt(var_delta3)
# Delta lower CI is -11.62347.

cat("The Confidence Interval of Delta is between", delta_upper3, "and", delta_lower3,
    "\nSince 0 is within the confidence interval, no significant improvement can be claimed.")


# Step 6 - Creating a confidence interval for Theta:
theta_upper3 <- theta3 + 1.96 * sqrt(var_theta3)
# Theta upper CI is 1.047536.

theta_lower3 <- theta3 - 1.96 * sqrt(var_theta3)
# Theta lower CI is 0.6673603.

cat("The Confidence Interval of Theta is between", theta_upper3, "and", theta_lower3,
    "\nSince 1 is within the confidence interval, no significant improvement can be claimed.")
#_______________________________________________________________________________________________



# Q4 ----
nb_model <- glm.nb(accidents ~ year, data = data)
summary(nb_model)


# Step 1 - Pi and Lambda:
pi4 <- predict(nb_model, newdata = data.frame(year = c(0)), type = "response")
# pi4 is 166.9381.


# Step 2 - Variance of Pi and Variance of Lambda:
## var(lambda) = lambda

var_pi4 <- as.numeric(predict(nb_model, newdata = data.frame(year = c(0)), type = "response",
                             se = TRUE)[2])^2
# var_p13 = 167.2901.


# Step 3 - Delta and Theta:
delta4 <- pi4 - lambda
# There is a decrease of 22.93807 in the number of accidents.

theta4 <- (lambda / pi4) / (1 + (var_pi4 / (pi4^2))) # 0.8574482:
# There is a relative decrease of 0.1425518 (1 - 0.8574482) in the number of accidents.


# Step 4 - Variance of Delta and Variance of Theta:
var_delta4 <- pi4 + lambda # 310.9381

var_theta4 <- (theta4^2) * ((var_pi4 / pi4^2) + (lambda / lambda^2)) /
  (1 + (var_pi4 / pi4^2))^2 # 0.009405832


# Step 5 - Creating a confidence interval for Delta:
delta_upper4 <- delta4 + 1.96 * sqrt(var_delta4)
# Delta upper CI is 57.4996.

delta_lower4 <- delta4 - 1.96 * sqrt(var_delta4)
# Delta lower CI is -11.62347.

cat("The Confidence Interval of Delta is between", delta_upper4, "and", delta_lower4,
    "\nSince 0 is within the confidence interval, no significant improvement can be claimed.")


# Step 6 - Creating a confidence interval for Theta:
theta_upper4 <- theta4 + 1.96 * sqrt(var_theta4)
# Theta upper CI is 1.047536.

theta_lower4 <- theta4 - 1.96 * sqrt(var_theta4)
# Theta lower CI is 0.6673602.

cat("The Confidence Interval of Theta is between", theta_upper4, "and", theta_lower4,
    "\nSince 1 is within the confidence interval, no significant improvement can be claimed.")
#_______________________________________________________________________________________________
