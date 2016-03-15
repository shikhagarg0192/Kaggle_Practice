library(stringr)
library(caret)
train1 <- read.csv("titanic/train.csv")
test1 <- read.csv("titanic/test.csv")
names(train1)
trn1 <- data.frame(survived = factor(train1$Survived))

t<-  str_match(train1$Name, "([A-Za-z]*),\\s([A-Za-z]*\\.)\\s(.*)")
d <- str_match(test1$Name, "([A-Za-z]*),\\s([A-Za-z]*\\.)\\s(.*)")

tst1 <- data.frame(rep(NA,nrow(test1)))

trn1$title[(t[,3]=="Mrs.")|(t[,3]=="Mme.")|(t[,3]=="Countess.")|(t[,3]=="Dona.")|(t[,3]=="Lady.")] <- "Missus"
trn1$title[(t[,3]=="Mr.")|(t[,3]=="Don.")|(t[,3]=="Jonkheer.")|(t[,3]=="Sir.")] <- "Mister"
trn1$title[(t[,3]=="Major.")|(t[,3]=="Col.")|(t[,3]=="Capt.")] <- "Mister"
trn1$title[(t[,3]=="Dr.")] <- "Doctor"
trn1$title[(t[,3]=="Rev.")] <- "Reverend"
trn1$title[(t[,3]=="Ms.")|(t[,3]=="Mlle.")|(t[,3]=="Miss.")] <- "Miss"
trn1$title[(t[,3]=="Master.")] <- "Master"
trn1$title[is.na(trn1$title)] = "Missus"


tst1$title[(d[,3]=="Mrs.")|(d[,3]=="Mme.")|(d[,3]=="Countess.")|(d[,3]=="Dona.")|(d[,3]=="Lady.")] <- "Missus"
tst1$title[(d[,3]=="Mr.")|(d[,3]=="Don.")|(d[,3]=="Jonkheer.")|(d[,3]=="Sir.")] <- "Mister"
tst1$title[(d[,3]=="Major.")|(d[,3]=="Col.")|(d[,3]=="Capt.")] <- "Mister"
tst1$title[(d[,3]=="Dr.")] <- "Doctor"
tst1$title[(d[,3]=="Rev.")] <- "Reverend"
tst1$title[(d[,3]=="Ms.")|(d[,3]=="Mlle.")|(d[,3]=="Miss.")] <- "Miss"
tst1$title[(d[,3]=="Master.")] <- "Master"
#tst1$title[is.na(tst1$title)] = "Missus"


tst1$title <- factor(tst1$title)
levels(tst1$title)

tst1$pclass <- factor(test1$Pclass)
tst1$sex <- factor(ifelse(test1$Sex=="male", 1, 0))


tst1$sex[test1$Sex=="male"] = 1
tst1$sex[test1$Sex=="female"] = 0
tst1$sibsp <- test1$SibSp
tst1$parch <- test1$Parch
tst1$embarked <- as.factor(test1$Embarked)

View(trn1)
str(trn1)
trn1$embarked <- as.character(train1$Embarked)
trn1$embarked[trn1$embarked==""]<-"S"
levels(trn1$embarked)
sum(trn1$embarked=="")
trn1$embarked <- as.factor(trn1$embarked)

trn1$title <- factor(trn1$title)
levels(trn1$title)

trn1$pclass <- factor(train1$Pclass)
trn1$sex <- factor(ifelse(train1$Sex=="male", 1, 0))

trn1$sex[train1$Sex=="male"] = 1
trn1$sex[train1$Sex=="female"] = 0
trn1$sibsp <- train1$SibSp
trn1$parch <- train1$Parch
trn1$fare <- train1$Fare
tst1$fare <- test1$Fare

View(trn1)
View(tst1)


trn1$age <- as.numeric(train1$Age)
tst1$age <- as.numeric(test1$Age)

sum(is.na(trn1$age))
sum(is.na(tst1$age))

trn1$age[is.na(trn1$age) & trn1$title == "Master"] <- mean(trn1$age[trn1$title == "Master"],na.rm = T)
trn1$age[is.na(trn1$age) & trn1$title == "Doctor"] <- mean(trn1$age[trn1$title == "Doctor"],na.rm = T)
trn1$age[is.na(trn1$age) & trn1$title == "Miss"] <- mean(trn1$age[trn1$title == "Miss"],na.rm = T)
trn1$age[is.na(trn1$age) & trn1$title == "Missus"] <- mean(trn1$age[trn1$title == "Missus"],na.rm = T)
trn1$age[is.na(trn1$age) & trn1$title == "Mister"] <- mean(trn1$age[trn1$title == "Mister"],na.rm = T)
trn1$age[is.na(trn1$age) & trn1$title == "Reverend"] <- mean(trn1$age[trn1$title == "Reverend"],na.rm = T)
    
tst1$age[is.na(tst1$age) & tst1$title == "Master"] <- mean(tst1$age[tst1$title == "Master"],na.rm = T)
tst1$age[is.na(tst1$age) & tst1$title == "Doctor"] <- mean(tst1$age[tst1$title == "Doctor"],na.rm = T)
tst1$age[is.na(tst1$age) & tst1$title == "Miss"] <- mean(tst1$age[tst1$title == "Miss"],na.rm = T)
tst1$age[is.na(tst1$age) & tst1$title == "Missus"] <- mean(tst1$age[tst1$title == "Missus"],na.rm = T)
tst1$age[is.na(tst1$age) & tst1$title == "Mister"] <- mean(tst1$age[tst1$title == "Mister"],na.rm = T)
tst1$age[is.na(tst1$age) & tst1$title == "Reverend"] <- mean(tst1$age[tst1$title == "Reverend"],na.rm = T)

tst1$age<- NULL

trn1$age<- NULL
apply(trn1, 2, function(x) sum(is.na(x)))
apply(tst1, 2, function(x) sum(is.na(x)))

tst1$rep.NA..nrow.test1.. <- NULL

str(trn1)
names(trn1)
names(train1)

View(tst1)

tst1$fare[is.na(tst1$fare)] <- 6.5

table(trn1$survived, trn1$sex)

idx = createDataPartition(trn1$survived, p = 0.7, list = F)
train <- trn1[idx,]
test <- trn1[-idx,]

names(trn1)
apply(tst1, 2, function(x)sum(is.na(x)))

str(trn1)
attach(train)
names(train)

#rpart tree
library(rpart)
t_rtee <- rpart(survived ~ ., data = train)
printcp(t_rtee)
plotcp(t_rtee)
plot(t_rtee)
text(t_rtee)
prd <- predict(t_rtee, test, type = "class")
table(test$survived, prd)
confusionMatrix(table(test$survived, prd))
which.min(t_rtee$cptable[,"xerror"])

t_cp <- t_rtee$cptable[3,"CP"]
p_tree <- prune(t_rtee, cp = t_cp)
plot(p_tree)
text(p_tree)
prd <- predict(p_tree, test, type = "class")
table(test$survived, prd)
confusionMatrix(table(test$survived, prd))



#ctree
library(party)
names(train)
t_ctree <- ctree(survived ~title+pclass+sex+sibsp+parch+fare+age, data = train)
t_ctree
plot(t_ctree)
text(t_ctree)
prd <- predict(t_ctree , test)

table(prd, test$survived)
1-mean(prd!=test$survived)

confusionMatrix(table(prd, test$survived))

final <- predict(t_ctree, tst1, type = "response")
head(final)

table(final)
write.csv(final,file = "submit.csv")
#min(t_ctree$)






#logistic2
trn_3 <- glm(survived~ pclass + sex + pclass:sex + age + sibsp + title + parch, family=binomial(logit), data = train)

summary(trn_3)
pred <- predict(trn_3, newdata = test, type = 'response')
#83.46

pred <- ifelse(pred > 0.5,1,0)
1-mean(pred!=test$survived)
confusionMatrix(table(pred,test$survived))

final <- ifelse(predict(trn_3, newdata = tst1, type = 'response')>0.5,1,0)
table(final)
write.csv(final,file = "submit.csv")

#train-test-tst1
#random forest
library(randomForest)

fit <- randomForest(survived~title+age+sex+fare+cabin+pclass+sibsp,data = train)
d <- importance(fit)
d

print(fit)

plot(fit)

tb <- ifelse(predict(fit, test) > 0.5,1,0)


confusionMatrix(table(tb, test$survived))


View(train)
#logistic regression
trn1$age20_40 <- 0
trn1$age20_40[trn1$age>20 & trn1$age<=40] <- 1
trn1$age20_40 <- as.factor(trn1$age20_40)

trn1$title_mr <- 0
trn1$title_mr[trn1$title == "Mister" & trn1$age20_40 == 1] <- 1
trn1$title_mr <- as.factor(trn1$title_mr)

trn1$pclass_23 <- 1
trn1$pclass_23[trn1$pclass == 1 & trn1$sex==0] <- 0

idx = createDataPartition(trn1$survived, p = 0.7, list = F)
train <- trn1[idx,]
test <- trn1[-idx,]

table(trn1$age,trn1$survived)

names(train)
mylogit <- glm(survived~pclass+sex+sibsp+age+embarked+fare, data = train, family = binomial)
summary(mylogit)
pred1 <- predict(mylogit, newdata = test, type = 'response')

pred <- ifelse(pred1 > 0.6,1,0)
test$predict <- pred
1-mean(pred!=test$survived)
confusionMatrix(table(pred,test$survived))

trn <- ifelse(predict(mylogit, newdata = tst1, type = 'response') > 0.69,1,0)
write.csv(trn, file = "submit.csv")
table(trn)
