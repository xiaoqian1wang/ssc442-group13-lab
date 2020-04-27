library(tidyverse)
h_data <- read.table("https://raw.githubusercontent.com/fivethirtyeight/data/master/hate-crimes/hate_crimes.csv", 
                    header=TRUE, sep=",")
view(h_data)
h_clean <- h_data[,sapply(h_data, is.numeric)]
h_data$hate_crimes_per_100k_splc[is.na(h_data$hate_crimes_per_100k_splc)] = 0

get_complexity = function(model) {
  length(coef(model)) - 1
}

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# part 1
attach(h_clean)

# selected models
mo_01 = lm(hate_crimes_per_100k_splc ~ share_voters_voted_trump + avg_hatecrimes_per_100k_fbi)
summary(mo_01)
c_01 = get_complexity(mo_01)
r_01 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_01))

mo_2 = lm(hate_crimes_per_100k_splc ~ median_household_income ,data=h_clean  )
summary(mo_2)
c_2 = get_complexity(mo_2)
r_2 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_2))

mo_3 = lm(hate_crimes_per_100k_splc ~ median_household_income + share_unemployed_seasonal)
summary(mo_3)
c_3 = get_complexity(mo_3)
r_3 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_3))

mo_4 = lm(hate_crimes_per_100k_splc ~ median_household_income + share_unemployed_seasonal + share_population_in_metro_areas)
summary(mo_4)
c_4 = get_complexity(mo_4)
r_4 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_4))

mo_5 = lm(hate_crimes_per_100k_splc ~ median_household_income +  share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree)
summary(mo_5)
c_5 = get_complexity(mo_5)
r_5 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_5))

mo_6 = lm(hate_crimes_per_100k_splc ~  share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree + share_non_citizen )
summary(mo_6)
c_6 = get_complexity(mo_6)
r_6 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_6))

mo_7 = lm(hate_crimes_per_100k_splc ~ share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree + share_non_citizen + share_white_poverty )
summary(mo_7)
c_7 = get_complexity(mo_7)
r_7 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_7))

mo_8 = lm(hate_crimes_per_100k_splc ~ share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index)
summary(mo_8)
c_8 = get_complexity(mo_8)
r_8 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_8))

mo_9 = lm(hate_crimes_per_100k_splc ~ share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index
          + share_non_white)
summary(mo_9)
c_9 = get_complexity(mo_9)
r_9 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_9))

mo_10 = lm(hate_crimes_per_100k_splc ~ share_unemployed_seasonal + share_population_in_metro_areas
           + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index
           + share_non_white + share_voters_voted_trump)
summary(mo_10)
c_10 = get_complexity(mo_10)
r_10 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_10))

mo_11 = lm(hate_crimes_per_100k_splc ~ share_unemployed_seasonal + share_population_in_metro_areas
           + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index
           + share_non_white + share_voters_voted_trump + avg_hatecrimes_per_100k_fbi)
summary(mo_11)
c_11 = get_complexity(mo_11)
r_11 = rmse(h_data$hate_crimes_per_100k_splc, predict(mo_11))

complexity <- c(c_01, c_2, c_3, c_4, c_5, c_6, c_7, c_8, c_9, c_10, c_11)
rmse <-       c(r_01, r_2, r_3, r_4, r_5, r_6, r_7, r_8, r_9, r_10, r_11)

plot(complexity, rmse, main="complexity vs rmse", xlab="complexity", ylab="RMSE", col="dark blue",type="p")

#part 2
library(rpart)
fit <- rpart(hate_crimes_per_100k_splc ~ share_unemployed_seasonal + share_population_in_metro_areas
             + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index
             + share_non_white + share_voters_voted_trump + avg_hatecrimes_per_100k_fbi,
             method= "anova")
printcp(fit)
plotcp(fit)


rsq.rpart(fit)

plot(fit, uniform=TRUE, pch=20, cex=8,
     main="Regression Tree for hate_crimes_per_100k_splc ")
text(fit, use.n=TRUE, all=TRUE, cex= 0.6)
