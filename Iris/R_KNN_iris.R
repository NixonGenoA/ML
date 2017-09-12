#outcome
#Train the model to predict the species type
#Load Library
library("caret")
#Load data
full.data<-read.csv("G:\\RStudio\\Datasets\\Iris Dataset\\Iris.csv")
str(full.data)
#Data Pre Processing
full.data$Id<-NULL
full.data[is.na(full.data),]

colnames(full.data)

cs<- preProcess(full.data,method = c("center","scale")) 
full.data.cs<-predict(cs,full.data)
#SepalLengthCm
ggplot(full.data)+
  geom_histogram(aes(SepalLengthCm,color=Species,fill=Species))
ggplot(full.data.cs)+
  geom_histogram(aes(SepalLengthCm,color=Species,fill=Species))

#SepalWidthCm
ggplot(full.data)+
  geom_histogram(aes(SepalWidthCm,color=Species,fill=Species))
#PetalLengthCm
ggplot(full.data)+
  geom_histogram(aes(PetalLengthCm,color=Species,fill=Species))
#PetalWidthCm
ggplot(full.data)+
  geom_histogram(aes(PetalWidthCm,color=Species,fill=Species))

ppv<- preProcess(full.data,method = c("center","scale","pca"))
full.data.t<-predict(ppv,full.data)


#Model
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

model <- train(Species~., data=full.data.t, trControl=train_control, method="knn")

summary(model)

prediction<- predict(model)

confusionMatrix(data=prediction, full.data.t$Species)

data <- table(prediction, full.data.t[,"Species"])

accuracy<-(50+44+44)/150
#0.92
write.csv(full.data.t[,"Species"],"result.csv")

