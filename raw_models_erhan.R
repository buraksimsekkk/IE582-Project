
require(dplyr)
require(caret)
require(Metrics)
require(glmnet)

IE582_Fall20_ProjectTrain <- read.csv("~/GitHub/Error_Loading/IE582_Fall20_ProjectTrain.csv", header=FALSE)
IE582_Fall20_ProjectTest <- read.csv("~/GitHub/Error_Loading/IE582_Fall20_ProjectTest.csv", header=FALSE)

train <- IE582_Fall20_ProjectTrain %>%
  mutate(V61 = ifelse(V61 == "b",0,1))

train <- train[-1,]

test <- IE582_Fall20_ProjectTest %>%
  mutate(V61 = ifelse(V61 == "b",0,1))

test <- test[-1,]


train1018 <- rbind(sample_n(filter(train, V61==1),509),filter(train, V61==0))

for (i in c(1,5:11,14,27.32)){
  train1018[,i] <- as.numeric(as.character(train1018[,i]))
}

train1018[,61] <- as.factor(train1018[,61])

set.seed(3488)
Index <- createDataPartition(train1018$V61, p = .7,
                                      list = FALSE,
                                      times = 1)
train1018_train <- train1018[Index,]
train1018_test <- train1018[-Index,]




#fit control
n_repeats=5
n_folds=10

fitControl=trainControl(method = "repeatedcv",
                        number = n_folds,
                        repeats = n_repeats)     

train_mat=as.matrix(train1018_train[,-61])
cvfit=cv.glmnet(train_mat,as.numeric(train1018_train$V61),family='binomial',nfolds=10, type.measure='class')
cvfit



lambda_min <- cvfit$lambda.min
lambda_min

lassofit <- glmnet(x=train_mat, y =train1018_train$V61, alpha = 1,lambda = lambda_min)

lasso_train_predicted <-round(predict(lassofit, train_mat, type = "class"))

accuracy(lasso_train_predicted,train1018_train$V61)

