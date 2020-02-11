library(tidyverse)
library(dplyr)
library(olsrr)
#lab 3 exercise 1
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

Ames <- select(ameslist, -c(OverallQual,OverallCond))
attach(Ames)
model_1 <- lm(SalePrice ~ LotFrontage+ LotArea + YearBuilt + GrLivArea + FullBath + BedroomAbvGr
              + KitchenAbvGr + Fireplaces + GarageArea + ScreenPorch + PoolArea + Heating +
                Functional + MasVnrArea)
a <- ols_step_best_subset(model_1)
plot(a)