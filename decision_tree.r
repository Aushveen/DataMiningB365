# Following instructions and approach from: https://www.guru99.com/r-decision-trees.html

library(dplyr)
library(rpart)
library(rpart.plot)

create_train_test <- function(data, size = 0.8, train = TRUE) {
    n_row = nrow(data)
    total_row = size * n_row
    train_sample <- 1: total_row
    if (train == TRUE) {
        return (data[train_sample, ])
    } else {
        return (data[-train_sample, ])
    }
}

# 1 Import data
set.seed(678)
path <- "/Users/sabrygateley/B365/course_proj/adult.csv"
adult_income <- read.csv(path)
# Ensure rows are shuffled so we randomly select for training / test data
shuffle_index <- sample(1:nrow(adult_income))
adult_income <- adult_income[shuffle_index, ]

# # 2 Without demographics
# no_sex <- adult_income %>% 
# # drop unnecessary variables
# select(-c(sex, age, race,marital.status,relationship))  %>% 
# # remove n/a observations
# na.omit()
# # 3 Split into testing and training data
# data_train_sex <- create_train_test(no_sex, 0.8, train = TRUE)
# data_test_sex <- create_train_test(no_sex, 0.8, train = FALSE)

# # 4 Build model 
# fit <- rpart(income~., data = data_train_sex, method = 'class')
# printcp(fit)
# rpart.plot(fit, extra = 106)

# # 5 Test
# predict_unseen <- predict(fit, data_test_sex, type = 'class')
# table_mat <- table(data_test_sex$income, predict_unseen)
# accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
# print("Accuracy without sex")
# print(accuracy_test)



# 2 Age
no_age <- adult_income %>% 
# drop unnecessary variables
select(-c(age))  %>% 
# remove n/a observations
na.omit()
#glimpse(clean_adult_income)
# 3 Split into testing and training data
data_train_age <- create_train_test(no_age, 0.8, train = TRUE)
data_test_age <- create_train_test(no_age, 0.8, train = FALSE)

# 4 Build model 
fit <- rpart(income~., data = data_train_age, method = 'class')
rpart.plot(fit, extra = 106)

# 5 Test
predict_unseen <- predict(fit, data_test_age, type = 'class')
table_mat <- table(data_test_age$income, predict_unseen)
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print("Accuracy without age")
print(accuracy_test)

# 2 Race
no_race <- adult_income %>% 
# drop unnecessary variables
select(-c(race))  %>% 
# remove n/a observations
na.omit()
# 3 Split into testing and training data
data_train_race <- create_train_test(no_race, 0.8, train = TRUE)
data_test_race <- create_train_test(no_race, 0.8, train = FALSE)

# 4 Build model 
fit <- rpart(income~., data = data_train_race, method = 'class')
rpart.plot(fit, extra = 106)

# 5 Test
predict_unseen <- predict(fit, data_test_race, type = 'class')
table_mat <- table(data_test_race$income, predict_unseen)
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print("Accuracy without race")
print(accuracy_test)


# 2 Sex
no_sex <- adult_income %>% 
# drop unnecessary variables
select(-c(sex))  %>% 
# remove n/a observations
na.omit()
# 3 Split into testing and training data
data_train_sex <- create_train_test(no_sex, 0.8, train = TRUE)
data_test_sex <- create_train_test(no_sex, 0.8, train = FALSE)

# 4 Build model 
fit <- rpart(income~., data = data_train_sex, method = 'class')
rpart.plot(fit, extra = 106)

# 5 Test
predict_unseen <- predict(fit, data_test_sex, type = 'class')
table_mat <- table(data_test_sex$income, predict_unseen)
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print("Accuracy without sex")
print(accuracy_test)


# 2 Education
no_edu <- adult_income %>% 
# drop unnecessary variables
select(-c(education))  %>% 
# remove n/a observations
na.omit()
# 3 Split into testing and training data
data_train_edu <- create_train_test(no_edu, 0.8, train = TRUE)
data_test_edu <- create_train_test(no_edu, 0.8, train = FALSE)

# 4 Build model 
fit <- rpart(income~., data = data_train_edu, method = 'class')
rpart.plot(fit, extra = 106)

# 5 Test
predict_unseen <- predict(fit, data_test_edu, type = 'class')
table_mat <- table(data_test_edu$income, predict_unseen)
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print("Accuracy without edu")
print(accuracy_test)


# 2 Relationship
no_rel <- adult_income %>% 
# drop unnecessary variables
select(-c(relationship))  %>% 
# remove n/a observations
na.omit()

# 3 Split into testing and training data
data_train_rel <- create_train_test(clean_adult_income, 0.8, train = TRUE)
data_test_rel <- create_train_test(clean_adult_income, 0.8, train = FALSE)

# 4 Build model 
fit <- rpart(income~., data = data_train_rel, method = 'class')
rpart.plot(fit, extra = 106)

# 5 Test
predict_unseen <- predict(fit, data_test_rel, type = 'class')
table_mat <- table(data_test_rel$income, predict_unseen)
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print("Accuracy without relationship")
print(accuracy_test)

clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
# select(-c(age, race, sex, education, occupation, fnlwgt, relationship))  %>% 
# remove n/a observations
na.omit()
# 3 Split into testing and training data
data_train <- create_train_test(clean_adult_income, 0.8, train = TRUE)
data_test <- create_train_test(clean_adult_income, 0.8, train = FALSE)

# 4 Build model 
fit <- rpart(income~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

# 5 Test
predict_unseen <- predict(fit, data_test, type = 'class')
table_mat <- table(data_test$income, predict_unseen)
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print("Accuracy for control group")
print(accuracy_test)

clean_adult_income <- adult_income %>% 
# drop unnecessary variables # REMOVE ALL NON-DEMOGRAPHIC
#select(-c(age, race, sex, education, relationship))  %>% 
select(-c(workclass, fnlwgt, education.num, education, occupation, capital.gain, capital.loss, hours.per.week, native.country))  %>% 
# remove n/a observations
na.omit()
#glimpse(clean_adult_income)
# 3 Split into testing and training data
data_train <- create_train_test(clean_adult_income, 0.8, train = TRUE)
data_test <- create_train_test(clean_adult_income, 0.8, train = FALSE)

# 4 Build model 
fit <- rpart(income~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

# 5 Test
predict_unseen <- predict(fit, data_test, type = 'class')
table_mat <- table(data_test$income, predict_unseen)
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print("Accuracy for only demographic group")
print(accuracy_test)