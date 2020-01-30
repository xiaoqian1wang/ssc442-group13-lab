library(tidyverse)
#Exercise 2
#1
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
Ames <- Filter(is.numeric,ameslist)
attach(ameslist)
simple.fit = lm(SalePrice ~ GarageType=='Builtin')
summary(simple.fit)

#2
attach(Ames)
predictor <-   MSSubClass+ LotFrontage + LotArea + OverallQual + OverallCond
+ YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF
+ TotalBsmtSF + X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath
+ BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd
+ Fireplaces + GarageYrBlt + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF
+ MoSold + YrSold
response <- c(SalePrice)
multi.fit = lm(response ~ predictor,data=Ames)
summary(multi.fit)

#3
par(mfrow=c(2,2))
plot(multi.fit)

#4
attach(Ames)
model_4_1 <- lm(SalePrice~OverallQual*OverallCond)
summary(model_4_1)

model_4_2 <- lm(SalePrice~ OverallQual+OverallCond+ OverallQual:OverallCond)
summary(model_4_2)


# 5
model_3 = lm(response ~ log(predictor, base=10))
summary(model_3)

model_4 = lm(response ~ sqrt(predictor))
summary(model_4)

model_5 = lm(response ~ predictor*predictor)
summary(model_5)


