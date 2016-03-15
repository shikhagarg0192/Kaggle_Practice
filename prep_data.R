prepare_Data <- function(data){
if("Survived" %in% names(data)) 
{
    d <- data.frame(Survived = factor(data$Survived))
}else
{
   d <- data.frame(Survived = rep(NA,nrow(data)))
}

d$class <- factor(data$Pclass)

d$Sex <- factor(data$Sex)
d$title<- rep(NA, nrow(data))

for(i in 1:nrow(data))
{
    if(length(grep("Mr\\.|Don\\.|Jonkheer\\.|Sir\\.",data[i,"Name"]))==1){
        d$title <- "Mister"
    }else if(length(grep("Dr\\.",data[i,"Name"]))==1){
        d$title <- "Doctor"
    }else if(length(grep("Major\\.|Col\\.|Capt\\.",data[i,"Name"]))==1){
        d$title <- "Soldier"
    }else if(length(grep("Rev\\.",data[i,"Name"]))==1){
        d$title <- "Reverend"
    }else if(length(grep("Mrs\\.|Mme\\.|Countess\\.|Dona\\.|Lady\\.",data[i,"Name"]))==1){
        d$title <- "Missus"
    }else if(length(grep("Miss\\.|Mlle\\.|Ms\\.",data[i,"Name"]))==1){
        d$title <- "Miss"
    }else if(length(grep("Master\\.",data[i,"Name"]))==1){
        d$title <- "Master"
    }
}

d$title <- factor(d$title)

d$Sibsp <- data$SibSp
d$parch <- data$Parch

d$age <- as.numeric(data$Age)

d$agecat <- apply(d, 1, function(row){
    age <- as.numeric(row["age"])
    title <- row["title"]
    if(is.na(age)){
        if(title=="Master"){
            return("[0,5)")
        }else{
            return("(15,60)")
        }
    }else if(age<5){
        return("[0,5)")
    }else if(age<=10){
        return("[5,10]")
    }else if(age<=15){
        return("(10,15]")
    }else if(age<60){
        return("(15,60)")
    }else if(age>=60){
        return("[60,.)")
    }else{
        return(NA)
    }
    
})

d$agecat <- factor(d$agecat)

d$ticket <- gsub("[^A-Z]","",data$Ticket)
d$ticket <- factor(ifelse(d$ticket=="","123",d$ticket))

d$fare <- data$Fare

d$Cabin <- gsub("[^A-Z]","",data$Cabin)
d$Cabin <- factor(ifelse(d$Cabin=="","Unknown", d$Cabin))

d$Embarked <- factor(data$Embarked)

return(d)
}
