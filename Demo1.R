table(train$Survived)
table(train$Pclass)
table(train$Sex)
# add “everyone dies” prediction to the test set dataframe
test$Survived = rep(0,418)
submit = data.frame(PassengerID = test$PassengerId,Survived = test$Survived)
write.csv(submit,file = "Prediction1.csv",row.names = F)