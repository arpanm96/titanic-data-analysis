#so it hides them a level deeper to maintain the rectangular types of containers that we are used to in things like spreadsheets, or now dataframes

#There's plenty of FamilyIDs with only one or two members, even though we wanted only family sizes of 3 or more.

# It will still have the factor level, but no actual observations of it in the set

train$Name[1]
test$Survived <- NA
# to bind the data sets of train and test simultaneously
combi <- rbind(train,test)
# to convert the default imported stringAsFactors to strings
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1],split='[,.]')[[1]][2]
combi$Title <- strsplit(combi$Name,split='[,.]')[[1]][2]
#to recursively send each name to strsplit 
combi$Title <- sapply(combi$Name,FUN = function(x){strsplit(x, split='[,.]')[[1]][2]})


combi$Title[1]
#to remove the extra space at the beg
combi$Title <- sub(" ","",combi$Title)
combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'

combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})
combi$FamilyId <- paste(as.character(combi$FamilySize),combi$Surname,sep = "")
combi$FamilyId[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyId)
famIDs <- data.frame(table(combi$FamilyId))
famIDs <- famIDs[famIDs$ Freq <= 2,]
# to overwrite any family IDs in our dataset for groups that were not correctly identified 
combi$FamilyId[combi$FamilyId %in% famIDs$Var1] <- 'Small'
combi$FamilyId <- factor(combi$FamilyId)
table(combi$FamilyId)

train <- combi[1:891,]
test <- combi[892:1309,]


# decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train,mehod="class")
plot(fit)
text(fit)

fancyRpartPlot(fit)
prediction <- predict(fit,test,method="class")
submit <- data.frame(PassengerId = test$PassengerId,Survived = prediction)
write.csv(submit,file ="Feature Engineering.csv",row.names = FALSE)
