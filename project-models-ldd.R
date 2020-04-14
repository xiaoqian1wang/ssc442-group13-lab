hate_crime <- read.table("https://raw.githubusercontent.com/fivethirtyeight/data/master/hate-crimes/hate_crimes.csv",
                       header = TRUE,
                       sep = ",")

tibble::as.tibble(hate_crime)
is.factor(hate_crime$state)
levels(hate_crime$state)

set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
hate_crime_idx = sample(nrow(hate_crime), round(nrow(hate_crime)/2))
hate_crime_trn = hate_crime[hate_crime_idx, ]
hate_crime_tst = hate_crime[-hate_crime_idx, ]


fit_caps = glm(state ~ avg_hatecrimes_per_100k_fbi, data = hate_crime_trn, family = binomial)
fit_selected = glm(state ~ median_household_income + share_unemployed_seasonal + share_population_in_metro_areas+ share_population_with_high_school_degree, data = hate_crime_trn, family = binomial)
fit_additive = glm(state ~ ., data= hate_crime_trn, family = binomial)
fit_over = glm(state ~ avg_hatecrimes_per_100k_fbi * (.), data = hate_crime_trn, family = binomial, maxit = 50)

# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "hate", "nonhate") != hate_crime$state)
mean(ifelse(predict(fit_selected) > 0, "hate", "nonhate") != hate_crime$state)
mean(ifelse(predict(fit_additive) > 0, "hate", "nonhate") != hate_crime$state)
mean(ifelse(predict(fit_over) > 0, "hate", "nonhate") != hate_crime$state)





