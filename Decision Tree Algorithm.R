#Decision Tree Algorithm

library(rpart)
fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method = "class")
plot(fit)
text(fit)

#loading the graphics packages
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#plot the fit data using rplot
fancyRpartPlot(fit)
# to create a survival prediction based on the fit data
Prediction <- predict(fit, test,type = "class")
submit <- data.frame(PassengerId = test$PassengerId,Survived = Prediction)
write.csv(submit,file = "Prediction_DTA.csv",row.names = FALSE)

#overfitting the data
fit2 <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method = "class",control = rpart.control(minsplit= 2,cp = 0))
fancyRpartPlot(fit2)

# custom decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control( control = rpart.control(minsplit= 2,cp = 0)))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
