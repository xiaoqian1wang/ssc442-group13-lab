library(tidyverse)
h_data <- read.table("https://raw.githubusercontent.com/fivethirtyeight/data/master/hate-crimes/hate_crimes.csv", 
                    header=TRUE, sep=",")
view(h_data)


# simple model
attach(h_data)
mo_1 = lm(hate_crimes_per_100k_splc ~ median_household_income + share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree + share_non_citizen + share_white_poverty + gini_index
          + share_non_white + share_voters_voted_trump + avg_hatecrimes_per_100k_fbi)
summary(mo_1)
# selected models
mo_2 = lm(hate_crimes_per_100k_splc ~ median_household_income   )
summary(mo_2)
mo_3 = lm(hate_crimes_per_100k_splc ~ median_household_income + share_unemployed_seasonal)
summary(mo_3)
mo_4 = lm(hate_crimes_per_100k_splc ~ median_household_income + share_unemployed_seasonal + share_population_in_metro_areas)
summary(mo_4)
mo_5 = lm(hate_crimes_per_100k_splc ~ median_household_income +  share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree)
summary(mo_5)
mo_6 = lm(hate_crimes_per_100k_splc ~  share_unemployed_seasonal + share_population_in_metro_areas
          + share_population_with_high_school_degree + share_non_citizen )
summary(mo_6)





