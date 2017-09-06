#install
install.packages("randomForest")
#load Library
library("randomForest")
#load Dataset
data<-read.csv("movie_metadata.csv")
#Data Pre processing
View(data)

data<-na.omit(data)
dim(data)
train<-data[1:2851,]
test<-data[2851:3801,]
colnames(train)
#model
model<-randomForest(imdb_score~gross+budget+content_rating,train)

summary(model)

#predict
predicted<-predict(model,test[,c("gross","budget","content_rating")])

write.csv(predicted,"result.csv")

