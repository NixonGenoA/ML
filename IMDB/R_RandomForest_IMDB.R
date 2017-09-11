#To predict imdb score using Random forest
  #imdb score range from 0-10

#install
#install.packages("randomForest")

#load Library
library("caret")
library("RANN")
#Data Pre processing
#load Dataset
 dataC<-read.csv("G:\\Rstudio\\Datasets\\IMDB\\movie_metadata.csv",na.strings = c(""," ",NA),stringsAsFactors =FALSE)
 View(dataC)
 
#cleaning
 dim(dataC)
 sum(is.na(dataC))
 clname<-colnames(dataC)[colSums(is.na(dataC)) > 0]
 # "color"                   "director_name"          
 # "num_critic_for_reviews"  "duration"               
 # "director_facebook_likes" "actor_3_facebook_likes" 
 # "actor_2_name"            "actor_1_facebook_likes" 
 # "gross"                   "actor_1_name"           
 # "actor_3_name"            "facenumber_in_poster"   
 # "plot_keywords"           "num_user_for_reviews"   
 # "language"                "country"                
 # "content_rating"          "budget"                 
 # "title_year"              "actor_2_facebook_likes" 
 # "aspect_ratio"
 
 dataC[is.na(dataC),]
 
 
 impute<-preProcess(method="knnImpute",dataC,na.remove = TRUE)
 dataC_Clean<-predict(impute,dataC)
 
  

dataC_Clean<-na.omit(dataC_Clean) 

str(dataC_Clean)

dataC_Clean$color<-as.factor(dataC_Clean$color)
dataC_Clean$movie_imdb_link <- NULL
dataC_Clean$director_name <- NULL
dataC_Clean$actor_1_name <- NULL
dataC_Clean$actor_2_name <- NULL
dataC_Clean$actor_3_name <- NULL
dataC_Clean$movie_title <- NULL

dmy <- dummyVars(" ~ .", data = dataC_Clean,fullRank = T)
dataC_transformed<-data.frame(predict(dmy, dataC_Clean))

View(dataC_transformed)

index<-createDataPartition(dataC_transformed$imdb_score,p=0.75,list=FALSE)
trainSet<-dataC_transformed[index,]
testSet<-dataC_transformed[-index,]



#Feature Selection
control<-rfeControl(functions = rfFuncs,method = "repeatedcv",repeats = 3)
trainp_fs<-dplyr::select(trainSet,-imdb_score)

fs<-rfe(trainp_fs,trainSet[,"imdb_score"],rfeControl=control)
fs

features <- c('actor_3_facebook_likes', 'actor_1_facebook_likes', 'gross',
              'num_voted_users', 'cast_total_facebook_likes', 'facenumber_in_poster',
              'num_user_for_reviews', 'budget', 'title_year',
              'actor_2_facebook_likes', 'aspect_ratio',
              'movie_facebook_likes')
target <- c('imdb_score')
#model<-randomForest(imdb_score~gross+budget+content_rating,train)
trainp_fs<-dplyr::select(trainSet,features)

model_fs<-train(trainp_fs,trainSet[,"imdb_score"],method = "rf")

#model
summary(model)

#predict
predicted<-predict(model,test[,c("gross","budget","content_rating")])

write.csv(predicted,"result.csv")

