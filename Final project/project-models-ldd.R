hate_crime <- read.table("https://raw.githubusercontent.com/fivethirtyeight/data/master/hate-crimes/hate_crimes.csv",
                       header = TRUE,
                       sep = ",")

tibble::as.tibble(hate_crime)
is.factor(hate_crime$state)
levels(hate_crime$state)

set.seed(42)
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

fit_caps_100 <- c()
fit_selected_100 <- c()
fit_additive_100 <- c()
fit_over_100 <- c()

# for loop for 100 fold 
for (i in 1:100) {
  #library(boot)
  #set.seed(1)
  caps_100 <- cv.glm(hate_crime_trn, fit_caps, K = 5)$delta[1]
  selected_100 <- cv.glm(hate_crime_trn, fit_selected, K = 5)$delta[1]
  additive_100 <- cv.glm(hate_crime_trn, fit_additive, K = 5)$delta[1]
  over_100 <- cv.glm(hate_crime_trn, fit_over, K = 5)$delta[1]
  
  
  fit_caps_100 <- append(fit_caps_100,caps_100)
  fit_selected_100 <- append(fit_selected_100,selected_100)
  fit_additive_100 <-append(fit_additive_100,additive_100)
  fit_over_100 <- append(fit_over_100,over_100)
}
mean(fit_caps_100)
mean(fit_selected_100)
mean(fit_additive_100)
mean(fit_over_100)


make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}


## fit_additive
hate_crime_pred = ifelse(predict(fit_additive, hate_crime_tst) > 0,
                       "hate",
                       "nonhate")
hate_crime_pred = ifelse(predict(fit_additive, hate_crime_tst, type = "response") > 0.5,
                       "hate",
                       "nonhate")


(conf_mat_50 = make_conf_mat(predicted = hate_crime_pred, actual = hate_crime_tst$state))
table(hate_crime_tst$state) / nrow(hate_crime_tst)

## fit_selected
hate_crime_pred = ifelse(predict(fit_selected, hate_crime_tst) > 0,
                       "hate",
                       "nonhate")
hate_crime_pred = ifelse(predict(fit_selected, hate_crime_tst, type = "response") > 0.5,
                       "hate",
                       "nonhate")

(conf_mat_50 = make_conf_mat(predicted = hate_crime_pred, actual = hate_crime_tst$state))
table(hate_crime_tst$state) / nrow(hate_crime_tst)

## fit_caps
hate_crime_pred = ifelse(predict(fit_caps, hate_crime_tst) > 0,
                       "hate",
                       "nonhate")
hate_crime_pred = ifelse(predict(fit_caps, hate_crime_tst, type = "response") > 0.5,
                       "hate",
                       "nonhate")

(conf_mat_50 = make_conf_mat(predicted = hate_crime_pred, actual = hate_crime_tst$state))
table(hate_crime_tst$state) / nrow(hate_crime_tst)
## fit_over
hate_crime_pred = ifelse(predict(fit_over, hate_crime_tst) > 0,
                       "hate",
                       "nonhate")
hate_crime_pred = ifelse(predict(fit_over, hate_crime_tst, type = "response") > 0.5,
                       "hate",
                       "nonhate")

(conf_mat_50 = make_conf_mat(predicted = hate_crime_pred, actual = hate_crime_tst$state))
table(hate_crime_tst$state) / nrow(hate_crime_tst)


