path = choose.files()
data = read.csv(path)

str(data)

# Checking for NA values:
any(is.na(data))
sum(is.na(data$age))
# There are 680 Na values in the data.

# Removal of NA values:
titanic_df = na.omit(data)
sum(is.na(titanic_df))
# There are no Na values in the data.

## Another option:
# titanic_df = data[!is.na(data$age), ]


# Q1 ----
# Creating a logistic regression model:
# The chance of survival according to age:
model1 = glm(formula = survived ~ age, data = titanic_df, family = binomial, )

summary(model1)
model1[1]
# y = (-0.01102924 * x) + 0.11719513


# Q2 ----
# The chance of survival according to age and sex:
model2 = glm(formula = survived ~ age + sex, data = titanic_df, family = binomial, )

summary(model2)
model2[1]


# The chance of survival by sex at ages 3, 30 and 60:
menPred = predict(model2, newdata = data.frame(sex = "male", age = c(5, 30, 60)),
                  type = "response")

womenPred = predict(model2, newdata = data.frame(sex = "female", age = c(5, 30, 60)),
                    type = "response")

cat("The chance of survival by sex at ages 5, 30 and 60:\nMen:",
    menPred, "\nWomen:", womenPred)

# The chance of survival by sex at ages 3, 30 and 60:
# Men: 0.2708611 0.2119384 0.1543468 
# Women: 0.8642732 0.8217461 0.757789