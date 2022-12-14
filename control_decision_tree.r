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

# # 2 Clean data
# clean_adult_income <- adult_income %>% 
# # DIFFERENCE BETWEEN BIASED AND UNBIASED
# # drop unnecessary variables
# select(-c(age, race, sex, education, occupation, fnlwgt, relationship))  %>% 
# remove n/a observations
# na.omit()
# #glimpse(clean_adult_income)

# # 3 Split into testing and training data
# data_train <- create_train_test(clean_adult_income, 0.8, train = TRUE)
# data_test <- create_train_test(clean_adult_income, 0.8, train = FALSE)

# # 4 Build model 
# fit <- rpart(income~., data = data_train, method = 'class')
# rpart.plot(fit, extra = 106)

# # 5 Test
# predict_unseen <- predict(fit, data_test, type = 'class')
# table_mat <- table(data_test$income, predict_unseen)
# accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
# print("Accuracy for control group")
# print(accuracy_test)

# select(-c(age, race, sex, education, occupation, fnlwgt, relationship, workclass, fnlwgt, education.num, education, occupation, capital.gain, capital.loss, hours.per.week, native.country))  %>% 

# 2 Clean data - AGE
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(race, sex, education, education.num, fnlwgt, relationship, workclass, fnlwgt, occupation, capital.gain, capital.loss, hours.per.week, native.country, marital.status))  %>% 
#remove n/a observations
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
print("Accuracy for only AGE group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - RACE
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(marital.status, education.num, age, sex, education, fnlwgt, relationship, workclass, fnlwgt, occupation, capital.gain, capital.loss, hours.per.week, native.country))  %>% 
#remove n/a observations
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
print("Accuracy for only RACE group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - EDUCATION
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(marital.status, education.num, age, sex, race, occupation, fnlwgt, relationship, workclass, fnlwgt, capital.gain, capital.loss, hours.per.week, native.country))  %>% 
#remove n/a observations
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
print("Accuracy for only EDUCATION group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - RELATIONSHIP
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(marital.status, education.num, age, sex, race, education, occupation, fnlwgt, workclass, fnlwgt, capital.gain, capital.loss, hours.per.week, native.country))  %>% 
#remove n/a observations
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
print("Accuracy for only RELATIONSHIP group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - NATIVE COUTNRY
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(marital.status, education.num, age, sex, race, occupation, education, fnlwgt, relationship, workclass, fnlwgt, capital.gain, capital.loss, hours.per.week))  %>% 
#remove n/a observations
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
print("Accuracy for only NATIVE CONTRY group")
print(accuracy_test)
printcp(fit)



# 2 Clean data - MARITAL STATUS
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(native.country, education.num, age, sex, race, occupation, education, fnlwgt, relationship, workclass, fnlwgt, capital.gain, capital.loss, hours.per.week))  %>% 
#remove n/a observations
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
print("Accuracy for only MARITAL STATUS group")
print(accuracy_test)
printcp(fit)


# 2 Clean data - OCCUPATION
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(native.country, education.num, age, sex, race, marital.status, education, fnlwgt, relationship, workclass, fnlwgt, capital.gain, capital.loss, hours.per.week))  %>% 
#remove n/a observations
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
print("Accuracy for only OCCUPATION group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - WORKCLASS
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(native.country, education.num, age, sex, race, marital.status, education, fnlwgt, relationship, occupation, fnlwgt, capital.gain, capital.loss, hours.per.week))  %>% 
#remove n/a observations
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
print("Accuracy for only WORKCLASS group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - CAPITAL GAIN
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(native.country, education.num, age, sex, race, marital.status, education, fnlwgt, relationship, occupation, fnlwgt, workclass, capital.loss, hours.per.week))  %>% 
#remove n/a observations
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
print("Accuracy for only CAPITAL GAIN group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - CAPITAL LOSS
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(native.country, education.num, age, sex, race, marital.status, education, fnlwgt, relationship, occupation, fnlwgt, workclass, capital.gain, hours.per.week))  %>% 
#remove n/a observations
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
print("Accuracy for only CAPITAL LOSS group")
print(accuracy_test)
printcp(fit)

# 2 Clean data - HOURS PER WEEK
clean_adult_income <- adult_income %>% 
# DIFFERENCE BETWEEN BIASED AND UNBIASED
# drop unnecessary variables
select(-c(native.country, education.num, age, sex, race, marital.status, education, fnlwgt, relationship, occupation, fnlwgt, workclass, capital.gain, capital.loss))  %>% 
#remove n/a observations
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
print("Accuracy for only HOURS PER WEEK group")
print(accuracy_test)
printcp(fit)