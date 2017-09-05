fullData<-read.csv("data.csv")

str(fullData)

View(fullData)

dim(fullData)
train<-fullData[1:427,-33]
test<-fullData[428:569,c(-2,-33)]

train<-na.omit(train)

library(rpart)

pred<-rpart(diagnosis~.,data=train,method = "class")

prediction<-predict(pred,test)

write.csv(prediction,"Result.csv")

summary(pred)

plot(pred,uniform = TRUE,main="TREE")
text(pred,use.n = TRUE,all=TRUE)

printcp(pred)
pred$cptable[which.min(pred$cptable[,"xerror"]),"CP"]
#prune(pred,cp=0.036723)

plot(pred,uniform = TRUE)
text(pred,all=TRUE,use.n=TRUE)

