source("prep_data.R")
library(party)
library(rattle)
library(rpart.plot)
train_data_Raw <- read.csv("C:/Users/shikhagarg.CORP/Downloads/ML with BD/kaggle/titanic/train.csv")
test_data_Raw <- read.csv("C:/Users/shikhagarg.CORP/Downloads/ML with BD/kaggle/titanic/test.csv")

train_data_prep <- prepare_Data(train_data_Raw)
test_data_prep <- prepare_Data(test_data_Raw)
attach(train_data_prep)

t <- ctree(Survived ~ class + Sex + title + Sibsp + parch + agecat + ticket + fare + Cabin + Embarked,
           data=train_data_prep, controls =  ctree_control(teststat = "quad",
                                          mincriterion = 0.95, 
                                          minsplit = 10,
                                          minbucket = 5,
                                          maxdepth = 0))

table(predict(t), train_data_prep$Survived)
sum1 <- 0
sum2 <-0
for(i in 1:length(t1)){
    sum1 <- sum1 + t1[i]}

f

plot(t, type = "simple")

test_p <- predict(t, newdata= test_data_prep)
