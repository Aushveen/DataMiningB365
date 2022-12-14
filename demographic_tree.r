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
# # drop unnecessary variables # REMOVE ALL NON-DEMOGRAPHIC
# #select(-c(age, race, sex, education, relationship))  %>% 
# #select(-c(workclass, fnlwgt, education.num, education, occupation, capital.gain, capital.loss, hours.per.week, native.country))  %>% 
# # remove n/a observations
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
# print("Accuracy for only demographic group")
# print(accuracy_test)
# printcp(fit)


# 2 Clean data
clean_adult_income <- adult_income %>% 
# drop unnecessary variables # REMOVE ALL NON-DEMOGRAPHIC
#select(-c(age, race, sex, education, relationship, education.num, occupation))  %>% 
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
printcp(fit)