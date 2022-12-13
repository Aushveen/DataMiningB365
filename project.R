x = read.csv2("C:/Users/aushv/OneDrive/Documents/B365 Final Project/Data.csv",stringsAsFactors=FALSE,sep=",")
library(dplyr)
rand <- sample_n(x,1700)
rand$age = as.numeric(rand$age)
rand$education.num = as.numeric(rand$education.num)
rand$race = as.numeric(rand$race)
rand$sex = as.numeric(rand$sex)
rand$hours.per.week = as.numeric(rand$hours.per.week)


# this is right below
library(e1071)
library(caTools)
library(class)

split <- sample.split(rand, SplitRatio = 0.7)
train_cl <- subset(rand, split == "TRUE")
test_cl <- subset(rand, split == "FALSE")

train_scale <- scale(train_cl[, 1:5])
test_scale <- scale(test_cl[, 1:5])

train_scaleAge <- scale(train_cl[, 1])
test_scaleAge <- scale(test_cl[, 1])

train_scaleEd <- scale(train_cl[, 2])
test_scaleEd <- scale(test_cl[, 2])

train_scaleRace <- scale(train_cl[, 3])
test_scaleRace <- scale(test_cl[, 3])

train_scaleSex <- scale(train_cl[, 4])
test_scaleSex <- scale(test_cl[, 4])

train_scaleHours <- scale(train_cl[, 5])
test_scaleHours <- scale(test_cl[, 5])




classifier_knn3 <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$income,
                      k = 3)

misClassError <- mean(classifier_knn3 != test_cl$income)
print(paste('Accuracy =', 1-misClassError))
print(misClassError)



classifier_knn3Age <- knn(train = train_scaleAge,
                       test = test_scaleAge,
                       cl = train_cl$income,
                       k = 3)
misClassErrorAge <- mean(classifier_knn3Age != test_cl$income)
print(paste('Accuracy =', 1-misClassErrorAge))
print(misClassErrorAge)



classifier_knn3Ed <- knn(train = train_scaleEd,
                       test = test_scaleEd,
                       cl = train_cl$income,
                       k = 3)
misClassErrorEd <- mean(classifier_knn3Ed != test_cl$income)
print(paste('Accuracy =', 1-misClassErrorEd))
print(misClassErrorEd)



classifier_knn3Race <- knn(train = train_scaleRace,
                       test = test_scaleRace,
                       cl = train_cl$income,
                       k = 3)
misClassErrorRace <- mean(classifier_knn3Race != test_cl$income)
print(paste('Accuracy =', 1-misClassErrorRace))
print(misClassErrorRace)



classifier_knn3Sex <- knn(train = train_scaleSex,
                       test = test_scaleSex,
                       cl = train_cl$income,
                       k = 3)
misClassErrorSex <- mean(classifier_knn3Sex != test_cl$income)
print(paste('Accuracy =', 1-misClassErrorSex))
print(misClassErrorSex)



classifier_knn3Hours <- knn(train = train_scaleHours,
                       test = test_scaleHours,
                       cl = train_cl$income,
                       k = 3)
misClassErrorHours <- mean(classifier_knn3Hours != test_cl$income)
print(paste('Accuracy =', 1-misClassErrorHours))
print(misClassErrorHours)

ans <- c(misClassErrorAge, misClassErrorEd, misClassErrorRace, misClassErrorSex, misClassErrorHours)
ans <- round(ans,2)
print(ans)