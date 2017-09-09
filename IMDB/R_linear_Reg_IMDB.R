#Load Library
library("caret")
#Load Data
dataFull<-read.csv("IMDB\\movie_metadata.csv",na.strings = c(""," ",NA))
View(dataFull)
#Data Pre processing and
#Data Exploration
sum(is.na(dataFull))
str(dataFull)
colnames(dataFull)
NULL->dataFull$movie_imdb_link
#Output Value 
 #imdb_score
 #Movie
   #color - type color
   #director_name 
   #Duration
   #gross
   #genra
   #actor_1_name ----actor_3_name
   #movie_title
   #plot keywords
   #movie_imdb_link
   #language
   #country
   #content_rating
   #budget
   #title_year
   #aspect_ration

 #Review
  #num_critics_for_review
  #director_facebook_likes
  #actor_1_facebook_likes....actor_3_facebook_likes
  #num_voted_users
  #cast_total_facebook_likes
  #facenumber_in_poster
  #num_users_for_review
  #movie_facebook_likes

max(dataFull$imdb_score)#10
pl.dt<-ggplot(dataFull)

#imdb_score vs color
dataFull$title_year[is.na(dataFull$color)]
dataFull$color[is.na(dataFull$color)]

dataFull$color[is.na(dataFull$color)]<-ifelse(dataFull$title_year[is.na(dataFull$color)] >= 1990,"Color","Black and White")
dataFull$color<-as.character(dataFull$color)
dataFull$color[is.na(dataFull$color)]<-"Color"
dataFull$color<-as.factor(dataFull$color)
pl.dt<-ggplot(dataFull)
pl.dt + geom_bar(aes(imdb_score,color=color,fill=color))
pl.dt + geom_boxplot(aes(color,imdb_score))
#b/w & color above average

#imdb_score vs Director Name
dataFull$director_name[dataFull$imdb_score > 8][1]
#Christopher Nolan
dataFull_Director<-dataFull[!is.na(data$director_name),]
pl.dt<-ggplot(dataFull_Director)
pl.dt + geom_bar(aes(reorder(director_name, -imdb_score),imdb_score),stat = "identity")+ coord_cartesian(xlim = c(0:20),ylim=c(0:11))
pl.dt + geom_area(aes(director_name,imdb_score))

#imdb_score vs Duration
pl.dt<-ggplot(dataFull)
pl.dt + geom_point(aes(imdb_score,duration,color=color,fill=color))+
  geom_smooth(aes(imdb_score,duration))
#best duration above 100

#imdb_score vs gross
pl.dt<-ggplot(dataFull)
pl.dt + geom_point(aes(imdb_score,gross,color=color,fill=color))+
  geom_smooth(aes(imdb_score,gross))
median(na.omit(dataFull$gross))
#below 1cr better chance of good rating

#feature Selection
ncol(dataFull)
dataFull<-na.omit(select(dataFull,c(-director_name)))
control<-rfeControl(functions=rfFuncs,method="cv",repeats = 4)
dim(predictor_feature)<-dplyr::select(dataFull,-imdb_score)
outcome_feature<-dataFull[,"imdb_score"]
fs<-rfe(predictor_feature,outcome_feature,rfeControl=control)
fs
#Model Training
train<-dataFull[1:950,]
test<-dataFull[950:1899,]


features <- c('actor_3_facebook_likes', 'actor_1_facebook_likes', 'gross',
              'num_voted_users', 'cast_total_facebook_likes', 'facenumber_in_poster',
              'num_user_for_reviews', 'budget', 'title_year',
              'actor_2_facebook_likes', 'aspect_ratio',
              'movie_facebook_likes')
target <- c('imdb_score')

x.feature <- as.data.frame(train[features])
y.target <- train$imdb_score

linear<- lm(y.target ~ ., x.feature)

#Model Testing
x.testfeature <- test[features]
colnames(x.testfeature)
predicted = predict(linear,x.feature) 

test$predicted<-predicted

#Results
write.csv(test$predicted,'result.csv')

