library(caret)
library(randomForest)
train1 <- read.csv("titanic/train.csv",stringsAsFactors = F)
test1 <- read.csv("titanic/test.csv",stringsAsFactors = F)
names(train1)

extractFeatures <- function(data){
    features <- c("Sex",
                  "Pclass",
                  "Fare",
                  "SibSp",
                  "Age",
                  "Parch",
                  "Embarked")
    fea <- data[,features]
    
    fea$Age[is.na(fea$Age)] <- -1
    fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
    fea$Embarked[fea$Embarked==""] = "S"
    fea$Sex      <- as.factor(fea$Sex)
    fea$Embarked <- as.factor(fea$Embarked)
    return(fea)
}

rf <- randomForest(extractFeatures(train1), as.factor(train1$Survived), ntree=100, importance=TRUE)

submission <- data.frame(PassengerId = test1$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test1))

write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)


imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff") +
    coord_flip() + 
    theme_light(base_size=20) +
    xlab("") +
    ylab("Importance") + 
    ggtitle("Random Forest Feature Importance\n") +
    theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)
