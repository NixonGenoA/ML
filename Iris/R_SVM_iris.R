full.data<-read.csv("Iris.csv")
View(full.data)
full.data<-na.omit(full.data)
dim(full.data)
train<-full.data[1:123,]
test<-full.data[124:150,-6]

#model
library("e1071")

model<-svm(train$Species~.,data=train)

summary(model)

prediction<-predict(model,test)

#confusion matrix
table(full.data[124:150,6],prediction)

result<-cbind(prediction=as.vector(prediction),data=full.data[124:150,6])

write.csv(result,"result.csv")

