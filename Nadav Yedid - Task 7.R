# Q1 ----
path = choose.files()
data_fire <- read.csv(path)

str(data_fire)
names(data_fire)

# Checking for NA values:
any(is.na(data_fire))
sum(is.na(data_fire))
# There are 6 Na values in the data.

data_fire <- na.omit(data_fire)
sum(is.na(data_fire))
# There are no Na values in the data.

data_fire[0:10, ]


# Q2 ----
# Creating a model with an exposure variable:
model_fire <- glm(fires ~ gdp_capita + region + offset(log(population)),
                  data = data_fire, family = poisson)

summary(model_fire)


# Q3 ----
B0 = as.numeric(model_fire$coefficients[1]) # Intercept
B1 = as.numeric(model_fire$coefficients[2]) # gdp_capita
B2 = as.numeric(model_fire$coefficients[3]) # Europe & Central Asia
B3 = as.numeric(model_fire$coefficients[4]) # Latin America & Caribbean
B4 = as.numeric(model_fire$coefficients[5]) # Middle East & North Africa
B5 = as.numeric(model_fire$coefficients[6]) # North America
B6 = as.numeric(model_fire$coefficients[7]) # South Asia
B7 = as.numeric(model_fire$coefficients[8]) # Sub-Saharan Africa

cat("The value of the B0 coefficient is:", B0,
"\n\nThe meaning of this coefficient is the number of fire cases that would have occurred
in East Asia and Pacipic if all the other parameters were equal to zero.")


# Q4 ----
p_value_0 = as.numeric(summary(model_fire)$coefficients["(Intercept)", "Pr(>|z|)"])
p_value_1 = as.numeric(summary(model_fire)$coefficients["gdp_capita", "Pr(>|z|)"])
p_value_2 = as.numeric(summary(model_fire)$coefficients["regionEurope & Central Asia", "Pr(>|z|)"])
p_value_3 = as.numeric(summary(model_fire)$coefficients["regionLatin America & Caribbean", "Pr(>|z|)"])
p_value_4 = as.numeric(summary(model_fire)$coefficients["regionMiddle East & North Africa", "Pr(>|z|)"])
p_value_5 = as.numeric(summary(model_fire)$coefficients["regionNorth America", "Pr(>|z|)"])
p_value_6 = as.numeric(summary(model_fire)$coefficients["regionSouth Asia", "Pr(>|z|)"])
p_value_7 = as.numeric(summary(model_fire)$coefficients["regionSub-Saharan Africa", "Pr(>|z|)"])

cat("It can be seen from the summary of the model that all the coefficients are distinct",
"\n\n P-value of Coefficient 0:", p_value_0,
    "\n P-value of Coefficient 1:", p_value_1,
    "\n P-value of Coefficient 2:", p_value_2,
    "\n P-value of Coefficient 3:", p_value_3,
    "\n P-value of Coefficient 4:", p_value_4,
    "\n P-value of Coefficient 5:", p_value_5,
    "\n P-value of Coefficient 6:", p_value_6,
    "\n P-value of Coefficient 7:", p_value_7,
    "\n\nIt can see that all the P-values are distinct.")


# Q5 ----
# Finding a predictive value for several parameters:
new_data <- data.frame(population = 10000, region = "Europe & Central Asia",
                       gdp_capita = 17000)

predict(model_fire, newdata = new_data, type = "response") # 2.390406


# Q6 ----
# Finding significance for the entire model:
1 - pchisq(q = model_fire$null.deviance - model_fire$deviance,
           df = model_fire$df.null - model_fire$df.residual) # 0 < 0.05
# The model is distinct.


# Q7.1 ----
# Sort the data by country name:
data_fire <- data_fire[order(data_fire$country), ]

# Create country_group column:
data_fire$country_group <- substr(data_fire$country, start = 1, stop = 1)

# Checking for NA values:
sum(is.na(data_fire))
# There are no Na values in the data.


# Q7.2 ----
# Create summarized data table:
summary_table <- data.frame(
  fires = tapply(data_fire$fires, data_fire$country_group, sum),
  
  population = tapply(data_fire$population, data_fire$country_group, sum),
  
  weighted_life_expectancy = tapply(data_fire$life_expectancy * data_fire$population,
    data_fire$country_group, sum) / tapply(data_fire$population, data_fire$country_group, sum))

# Change row names to country group letters:
row.names(summary_table) <- unique(data_fire$country_group)
colnames(summary_table) <- c("fires", "population", "weighted_life_expectancy")

# Create fires_per_million variable:
summary_table$fires_per_million <- (summary_table$fires / summary_table$population) * 1e6

summary_table$fires_per_million <- round(summary_table$fires_per_million, 5)

str(summary_table)

# Checking for NA values:
sum(is.na(summary_table))
# There are no Na values in the data.

# Create the model:
library(MASS)

# A model for predicting the number of fires for a group of countries per year per million people,
# Depending on the predictor of average life expectancy of a single person for each group of countries:
model_fire_7 <- glm.nb(fires_per_million ~ weighted_life_expectancy, data = summary_table)

summary(model_fire_7)

B0_2 = as.numeric(model_fire_7$coefficients[1]) # Intercept
B1_2 = as.numeric(model_fire_7$coefficients[2]) # weighted_life_expectancy  


# In order to find the lambda value, 70 was chosen as the number of weighted life expectancy.
predict_lambda = predict(model_fire_7, newdata = data.frame(weighted_life_expectancy = 70),
                        type = "response") # 59.25578

manually_lambda = exp(B0_2 + B1_2 * 70) # 59.25578

numeric_lambda = exp(-4.211 + 0.1184695 * 70) # 59.25588

cat("Predicted lambda:", predict_lambda,
    "\nManually lambda:", manually_lambda,
    "\nNumeric lambda:", numeric_lambda,
    "\nIt can be seen that the lambda values are the same.",
    "\nThe model equation is: e^(-4.211 + (0.1184695 * B1))")


#**********************************************
# Finding significance for the entire model:
1 - pchisq(q = model_fire_7$null.deviance - model_fire_7$deviance,
           df = model_fire_7$df.null - model_fire_7$df.residual) # 0.006784313 < 0.05
# The model is distinct.
#**********************************************
