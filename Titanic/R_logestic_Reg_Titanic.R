train<-read.csv("train.csv")
test<-read.csv("test.csv")
train<-na.omit(train)
test<-na.omit(test)

colnames(train)
colnames(test)

logestic<-glm(Survived ~ .,data=train,family="binomial")

predicted<-predict(logestic,test)
predicted

summary(predicted)

write.csv(predicted,'test.csv')




