library(Amelia)
require(Amelia)
library(stringr)
library(party)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(train)
train$Pclass <- factor(train$Pclass)
train$Survived <- factor(train$Survived)
test$Pclass <- factor(test$Pclass)


#train$Sex <- factor(train$Sex)
#train$Embarked <- factor(train$Embarked)

sum(is.na(train$Age)==TRUE)
sum(is.na(test$Age) | is.nan(test$Age))
test[is.na(test$Age),]

sum(is.na(train$Age)==TRUE)/length(train$Age) *100
sapply(train, function(df){sum(is.na(df)==TRUE)/length(df) * 100})

#missmap(train, main="Missing values")

#AmeliaView()

train$Name <- as.character(train$Name)
#s means space, tab, new lines
wrds <- table(unlist(strsplit(train$Name,'\\s+')))
sort(wrds[grep('\\.',names(wrds))], decreasing = TRUE)

tb <- cbind(train$Age,str_match(train$Name, "[a-zA-Z]+\\."))
table(tb[is.na(tb[,1]),2])

mean.mr = mean(train$Age[grepl("Mr\\.",train$Name) & !is.na(train$Age)])
mean.dr = mean(train$Age[grepl("Dr\\.",train$Name) & !is.na(train$Age)])
mean.master = mean(train$Age[grepl("Master\\.",train$Name) & !is.na(train$Age)])
mean.miss = mean(train$Age[grepl("Miss\\.",train$Name) & !is.na(train$Age)])
mean.mrs = mean(train$Age[grepl("Mrs\\.",train$Name) & !is.na(train$Age)])

train$Age[grepl("Mr\\.",train$Name) & is.na(train$Age)] <- mean.mr
train$Age[grepl("Dr\\.",train$Name) & is.na(train$Age)] <- mean.dr
train$Age[grepl("Master\\.",train$Name) & is.na(train$Age)] <- mean.master
train$Age[grepl("Miss\\.",train$Name) & is.na(train$Age)] <- mean.miss
train$Age[grepl("Mrs\\.",train$Name) & is.na(train$Age)] <- mean.mrs



test$Name <- as.character(test$Name)
#s means space, tab, new lines
wrds2 <- table(unlist(strsplit(test$Name,'\\s+')))
sort(wrds2[grep('\\.',names(wrds2))], decreasing = TRUE)

tb2 <- cbind(test$Age,str_match(test$Name, "[a-zA-Z]+\\."))
table(tb2[is.na(tb2[,1]),2])

mean.mr2 = mean(test$Age[grepl("Mr\\.",test$Name) & !is.na(test$Age)])
mean.ms2 = mean(test$Age[grepl("Dona\\.",test$Name)])
mean.master2 = mean(test$Age[grepl("Master\\.",test$Name) & !is.na(test$Age)])
mean.miss2 = mean(test$Age[grepl("Miss\\.",test$Name) & !is.na(test$Age)])
mean.mrs2 = mean(test$Age[grepl("Mrs\\.",test$Name) & !is.na(test$Age)])

test$Age[grepl("Mr\\.",test$Name) & is.na(test$Age)] <- mean.mr2
test$Age[grepl("Ms\\.",test$Name) & (is.na(test$Age) | is.nan(test$Age))] <- mean.ms2
test$Age[grepl("Master\\.",test$Name) & is.na(test$Age)] <- mean.master2
test$Age[grepl("Miss\\.",test$Name) & is.na(test$Age)] <- mean.miss2
test$Age[grepl("Mrs\\.",test$Name) & is.na(test$Age)] <- mean.mrs2

barplot(table(train$Survived), main = "survival", names = c("dead", "Survived"))
barplot(table(train$Pclass), main = "Passenger class", names = c("first", "second", "third"))
barplot(table(train$Sex), main = "gender")
hist(train$Age, main = "Age", xlab = "Age")
barplot(table(train$SibSp), main = "Sibling and spouse")
barplot(table(train$Parch), main = "Parents and child")
hist(train$Fare, main = "Fare", xlab = "Fare")
barplot(table(train$Embarked), main = "Port of embarkment")

counts = table(train$Survived, train$Sex)
barplot(counts, col = c("darkblue", "red"), legend = c("Dead", "Survived"), main = "Survival depending on gender")
counts = table(train$Survived, train$Pclass)
barplot(counts, col = c("darkblue", "red"), legend = c("Dead", "Survived"), main = "Survival depending on gender")

ctree_train <- ctree(Survived ~ Pclass + Sex + Age + SibSp + Fare + Parch + Embarked, data = train)
ctree_train
plot(ctree_train)

test_p = predict(ctree_train, test$Survived)
str(test_p)
length(test_p)

str(test)


