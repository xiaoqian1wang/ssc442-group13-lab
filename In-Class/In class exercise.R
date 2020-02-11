# In class exercise
library(tidyverse)
bank <- read_csv("bank.csv")
bank_data <- Filter(is.numeric, bank)
reg_b <- lm(balance ~ age+day+duration+campaign+previous, data=bank_data)
coeff_b = coefficients(reg_b)

add_column(bank_data, a=0, .before=1)
attach(bank_data)
plot(age+day+duration+campaign+previous, balance, col="orange")
abline(reg_b,col="black")
predict(reg_b)
null_bank_model = lm(balance ~ 1, data = bank_data)
full_bank_model = lm(balance ~ age+day+duration+campaign+previous, data=bank_data)
anova(null_bank_model, full_bank_model)
summary(reg_b)

# Through codes, we can remove "day", "duration", "campaign" and "previous" according to F test.