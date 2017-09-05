data<-read.csv("movie_metadata.csv")
colnames(data)

data<-na.omit(data)
head(data)
nrow(data)#3801
train<-data[1:950,]
test<-data[950:1899,]
nrow(train)
nrow(test)

features <- c('actor_3_facebook_likes', 'actor_1_facebook_likes', 'gross',
            'num_voted_users', 'cast_total_facebook_likes', 'facenumber_in_poster',
            'num_user_for_reviews', 'budget', 'title_year',
            'actor_2_facebook_likes', 'aspect_ratio',
            'movie_facebook_likes')
target <- c('imdb_score')

x.feature <- as.data.frame(train[features])
y.target <- train$imdb_score

linear<- lm(y.target ~ ., x.feature)
x.testfeature <- test[features]
colnames(x.testfeature)
predicted = predict(linear,x.feature) 

test$predicted<-predicted

write.csv(predicted,'test.csv')

