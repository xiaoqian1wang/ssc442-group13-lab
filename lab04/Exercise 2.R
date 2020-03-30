# Exercise 2
library(boot)
library(caret)
bank <- read.csv("bank.csv")

tibble::as.tibble(bank)

is.factor(bank$y)
levels(bank$y)

set.seed(42)
bank_idx = sample(nrow(bank), round(nrow(bank)/2))
bank_trn = bank[bank_idx, ]
bank_tst = bank[-bank_idx, ]

fit_caps = glm(y ~ balance, data = bank_trn, family = binomial)
fit_selected = glm(y ~ age + education + balance+ default, data = bank_trn, family = binomial)
fit_additive = glm(y ~ ., data= bank_trn, family = binomial)
fit_over = glm(y ~ balance * (.), data = bank_trn, family = binomial, maxit = 50)

set.seed(1)
cv.glm(bank_trn, fit_caps, K = 10)$delta[1]
cv.glm(bank_trn, fit_selected, K = 10)$delta[1]
cv.glm(bank_trn, fit_additive, K = 10)$delta[1]
cv.glm(bank_trn, fit_over, K = 10)$delta[1]


coef(fit_caps)
coef(fit_selected)
coef(fit_additive)
coef(fit_over)
summary(fit_selected)

predict(fit_caps,newdata = bank_tst, type = "response")

table(bank_tst$y, predict(fit_caps,newdata = bank_tst, type = "response")>0.15)
