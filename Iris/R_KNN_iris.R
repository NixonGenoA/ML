data<-read.csv("Iris.csv")
dim(data)

train<-data[1:100,]

test<-data[100:150,-"Species"]

#model
library(knn)

model<-knn(Species~.,train)

pred<-predict(model,test)

write.csv(pred,"result.csv")




