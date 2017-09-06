fullData<-read.csv("G:\\RStudio\\Datasets\\Delhi Weather\\testset.csv")
View(fullData)

dim(fullData)

train<-na.omit(fullData[1:8000,c(-"X_precipm",-"X_windchillm",-"X_wgustm")])
test<-na.omit(fullData[8000:9000,c(-"X_precipm",-"X_windchillm",-"X_wgustm")])

#model
library(e1071)
fit<-naiveBayes(X_conds~.,data=train)

summary(fit)

#predict
prediction<-predict(fit,test)
#NEED TO IMPROVE DATA FOR ACCURATE PREDICTION
cbind(pred=as.character.factor(prediction),test=as.character.factor(test$X_conds))