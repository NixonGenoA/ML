# load the library
library(caret)
# load the iris dataset
data(iris)
# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3,search="random")
# train the model
model_1 <- train(Species~., data=iris, trControl=train_control, method="nb")
model_2 <- train(Species~., data=iris,metric="Accuracy", tuneLength=15, trControl=train_control, method="rf")

# summarize results
print(model_1)
print(model_2)

plot(model_1)
plot(model_1)