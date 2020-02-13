##############################################################
#   lab 03 Group 13.
#   Group members: Xinyu Li, Nick Carroll, Xiaoqian Wang, Dongdong Li, 
###############################################################
# exercise 1
library(tidyverse)
library(dplyr)
library(olsrr)
library(GuessCompx)
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",", na.strings=TRUE,stringsAsFactors = TRUE)

Ames <- select(ameslist, -c(OverallQual,OverallCond))
Ames_clean <- Ames[,sapply(Ames, is.numeric)]
sapply(Ames, function(Ames){ifelse(is.na(Ames), 0, Ames)})

get_complexity = function(model) {
  length(coef(model)) - 1
}

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# model 1
mo_1 <- lm(SalePrice ~ . , data=Ames_clean)
c_1 <- get_complexity(mo_1)
r_1 <- rmse(SalePrice, predict(mo_1))
c_1
r_1
a <- ols_step_forward_p(mo_1)
a
summary(mo_1)
# model 2
mo_2 <- lm(SalePrice ~  GarageCond + TotalBsmtSF +BsmtExposure + Fireplaces, data=Ames)
c_2 <- get_complexity(mo_2)
c_2
r_2 <- rmse(SalePrice, predict(mo_2))
r_2
b <- ols_step_forward_p(mo_2)
b
# model 3
attach(Ames)
mo_3 <- lm(SalePrice ~ Heating+FireplaceQu+ BsmtExposure)
c_3 <- get_complexity(mo_3)
c_3
r_3 <- rmse(SalePrice, predict(mo_3))
r_3
c <- ols_step_forward_p(mo_3)
c
# model 4
mo_4 <- lm(SalePrice ~ HouseStyle + HeatingQC + KitchenQual + GarageArea, data=Ames)
c_4 <- get_complexity(mo_4)
c_4
r_4 <- rmse(SalePrice, predict(mo_4))
r_4
d <- ols_step_forward_p(mo_4)
d
# model 5
mo_5 <- lm(SalePrice ~ Street + CentralAir + YearBuilt + GarageQual + GrLivArea + Utilities, data=Ames)
c_5 <- get_complexity(mo_5)
c_5
r_5 <- rmse(SalePrice, predict(mo_5))
r_5
e <- ols_step_forward_p(mo_5)
e
# model 6
mo_6 <- lm(SalePrice ~ PoolArea+Foundation+ExterCond+Alley + LotFrontage + MasVnrType + FullBath + GarageFinish, data=Ames)
c_6 <- get_complexity(mo_5)
c_6
r_6 <- rmse(SalePrice, predict(mo_6))
r_6
f <- ols_step_forward_p(mo_6)
f
# model 7
mo_7 <- lm(SalePrice ~  OpenPorchSF+ BsmtExposure + WoodDeckSF+ LowQualFinSF, data=Ames)
c_7 <- get_complexity(mo_7)
c_7
r_7 <- rmse(SalePrice, predict(mo_7))
r_7
g <- ols_step_forward_p(mo_7)
g
# model 8
mo_8 <- lm(SalePrice ~ MoSold + YrSold+ GarageCars+ KitchenAbvGr+LotConfig+RoofStyle, data=Ames)
c_8 <- get_complexity(mo_8)
c_8
r_8 <- rmse(SalePrice, predict(mo_8))
r_8
h <- ols_step_forward_p(mo_8)
h
# model 9
mo_9 <- lm(SalePrice ~ Electrical + RoofMatl+ LandSlope, data=Ames)
c_9 <- get_complexity(mo_9)
c_9
r_9 <- rmse(SalePrice, predict(mo_9))
r_9
i <- ols_step_forward_p(mo_9)
i
# model 10
mo_10 <- lm(SalePrice ~  ExterQual+ BldgType+ GarageQual, data=Ames)
c_10 <- get_complexity(mo_10)
c_10
r_10 <- rmse(SalePrice, predict(mo_10))
r_10
j <- ols_step_forward_p(mo_10)
j
# model 11
mo_11 <- lm(SalePrice ~BsmtExposure+ HeatingQC+ FireplaceQu, data=Ames)
c_11 <- get_complexity(mo_11)
c_11
r_11 <- rmse(SalePrice, predict(mo_11))
r_11
k <- ols_step_forward_p(mo_11)
k
# model 12
mo_12 <- lm(SalePrice ~ BldgType+MasVnrType+ BsmtHalfBath+LowQualFinSF, data=Ames)
c_12 <- get_complexity(mo_12)
c_12
r_12 <- rmse(SalePrice, predict(mo_12))
r_12
l <- ols_step_forward_p(mo_12)
l
#model 13
mo_13 <- lm(SalePrice ~ BsmtCond+GarageCond+ExterCond, data=Ames)
c_13 <- get_complexity(mo_13)
c_13
r_13 <- rmse(SalePrice, predict(mo_13))
r_13
m <- ols_step_forward_p(mo_13)
m
# model 14
mo_14 <- lm(SalePrice ~ RoofMatl+ BsmtFullBath+ Electrical+HalfBath, data=Ames)
c_14 <- get_complexity(mo_14)
c_14
r_14 <- rmse(SalePrice, predict(mo_14))
r_14
n <- ols_step_forward_p(mo_14)
n
# model 15
mo_15 <- lm(SalePrice ~ EnclosedPorch+ RoofStyle+ ScreenPorch+FullBath, data=Ames)
c_15 <- get_complexity(mo_15)
c_15
r_15 <- rmse(SalePrice, predict(mo_15))
r_15
o <- ols_step_forward_p(mo_15)
o

x <- c(c_1, c_2, c_3, c_4, c_5,c_6,c_7,c_8,c_9,c_10,c_11,c_12,c_13,c_14,c_15)
x
y <- c(r_1, r_2, r_3, r_4, r_5,r_6,r_7,r_8,r_9,r_10,r_11,r_12,r_13,r_14,r_15)
y
t <- data.frame(x,y)

t

plot(data=t,x,y,main="complexity vs rmse", xlab="complexity", ylab="RMSE", col="dark blue")

#exercise 2

Ames <- select(ameslist, -c(OverallQual,OverallCond))


Ames = Ames[complete.cases(Ames[ , c(4,5,18,25,45,48,50,51,54,55,61,69,70,38)]),]

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


get_complexity = function(model) {
  length(coef(model)) - 1
}

set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]


############

fit_1 = lm(SalePrice ~ LotFrontage+ LotArea + YearBuilt, data = train_data)
fit_2 = lm(SalePrice ~ GrLivArea + FullBath + BedroomAbvGr, data = train_data)
fit_3 = lm(SalePrice ~ KitchenAbvGr + Fireplaces+ GarageArea, data = train_data)
fit_4 = lm(SalePrice ~ ScreenPorch + PoolArea + Heating, data = train_data)
fit_5 = lm(SalePrice ~ Functional + MasVnrArea, data = train_data)
get_complexity(fit_5)


# train RMSE
rmse(actual = train_data$SalePrice, predicted = predict(fit_5, train_data))
# test RMSE
rmse(actual = test_data$SalePrice, predicted = predict(fit_5, test_data))

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}


model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5)

train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)


# This is the same as the apply command above

test_rmse = c(get_rmse(fit_1, test_data, "SalePrice"),
              get_rmse(fit_2, test_data, "SalePrice"),
              get_rmse(fit_3, test_data, "SalePrice"),
              get_rmse(fit_4, test_data, "SalePrice"),
              get_rmse(fit_5, test_data, "SalePrice"))



plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE",)
lines(model_complexity, test_rmse, type = "b", col = "darkorange")


