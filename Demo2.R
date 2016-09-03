# summary
summary(train$Sex)
# comparison on the number of males and females that survived
prop.table(table(trainDF$SEX,trainDF$SURVIVED))
# comparison on the number of males and females that survived row-based(1)
prop.table(table(trainDF$SEX,trainDF$SURVIVED),1)
test$Survived <- 0
# create a subset of the total dataframe, and apply our assignment of “1” to only those rows that meet the criteria specified
test$Survived[test$Sex == "female"] <- 1

summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1

#to find the proportion of the datasets to the target variable
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train,FUN = function(x){sum(x)/length(x)})

hist(train$Fare)
train$Fare2 = '30+'
train$Fare2[train$Fare < 30 && train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 && train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
aggregate(Survived ~ Fare2+Pclass+Sex+Child,data=train,FUN = function(x){sum(x)/length(x)})


test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1
test$Survived[test$Sex=='female' & test$Pclass==3 & test$Fare >= 20] <- 0

submit = data.frame(PassengerID = test$PassengerId,Survived = test$Survived)
write.csv(submit,file="Prediction2.csv",row.names = FALSE)