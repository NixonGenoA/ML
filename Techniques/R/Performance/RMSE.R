# load libraries
library(caret)
# load data
data(longley)
# prepare resampling method
control <- trainControl(method="cv", number=5)

fit <- train(Employed~., data=longley, method="lm", metric="RMSE", trControl=control)
# display results
print(fit)