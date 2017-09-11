#Libraray
library("caret")
library("dplyr")
library("corrgram")
#Load Dataset
fullData<-read.csv("G:\\RStudio\\Datasets\\Kaggle BCD\\data.csv")
str(fullData)
View(fullData)
dim(fullData)
#Data Pre Process
NULL->fullData$X
NULL->fullData$id
nrow(fullData[(is.na(fullData)),])

corrgram(fullData,order = TRUE,upper.panel=panel.cor,
         diag.panel=panel.density)
without_cat<-fullData[,c(-1)]
c<-cor(fullData[,c(-1)])
hc <- findCorrelation(c, cutoff=0.3) # put any value as a "cutoff" 

fullData_c <- cbind(without_cat[,hc],diagnosis = fullData[,"diagnosis"])
View(fullData_c)

#concavity mean
ggplot(fullData_c)+
  geom_histogram(aes(concavity_mean,color=diagnosis,fill=diagnosis))+
  geom_freqpoly(aes(concavity_mean),color="red")

#concavity_worst
ggplot(fullData_c)+
  geom_histogram(aes(concavity_worst,color=diagnosis,fill=diagnosis))+
  geom_freqpoly(aes(concavity_worst),color="red")

#radius_mean
ggplot(fullData_c)+
  geom_histogram(aes(radius_mean,color=diagnosis,fill=diagnosis))+
  geom_freqpoly(aes(radius_mean),color="red")

#fractal_dimension_worst
ggplot(fullData_c)+
  geom_histogram(aes(fractal_dimension_worst,color=diagnosis,fill=diagnosis))+
  geom_freqpoly(aes(fractal_dimension_worst),color="red")

#radius_se vs area_Se
ggplot(fullData_c)+
  geom_point(aes(radius_se,area_se,color=diagnosis,fill=diagnosis))

#radius_se vs area_Se
ggplot(fullData_c)+
  geom_point(aes(radius_se,area_se,color=diagnosis,fill=diagnosis))

#fractal_dimension_worst vs symmetry_worst
ggplot(fullData_c)+
  geom_point(aes(fractal_dimension_worst,symmetry_worst,color=diagnosis,fill=diagnosis))

#Dummy Variable
dumy<-dummyVars("~.",fullData_c,fullRank = TRUE)
newData<-as.data.frame(predict(dumy,fullData_c))


#partition
index<-createDataPartition(newData$diagnosis,p=0.75,list = FALSE)
trainSet<-newData[index,]
testSet<-newData[index,]

#model
colnames(trainSet)
model<-train(trainSet[,-27],trainSet[,"diagnosis.M"],method = "rpart")

predictor<-predict(model,testSet[,-27])

write.csv(predictor,"Result.csv")

summary(predictor)

