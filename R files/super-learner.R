library(SuperLearner)
library(fastDummies)

ksdf = read.csv("cleaned-ks-data-more.csv", header = TRUE)
data = dummy_cols(ksdf, select_columns = c("category", "launched"))
data = subset(data, select = c(-category, -launched, -X.1, -X))

predict_variable = data$state01

data = subset(data, select = -state01)

train_obs = sample(nrow(data), 50000)
X_train = data[train_obs, ]
X_test = data[-train_obs, ]
Y_train = predict_variable[train_obs]
Y_test = predict_variable[-train_obs]

#Super Learner Models

#Random Forest
sl_rf = sl_rf = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                             SL.library = "SL.randomForest")

#Lasso
sl_lasso = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                        SL.library = "SL.glmnet")


#3 model comparison
sl_3 = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                                    SL.library = c("SL.mean", "SL.glmnet", "SL.randomForest"))
sl_3

#Predictions
predictions <- predict.SuperLearner(sl_3, newdata=X_test)
library(dplyr)
conv.preds <- ifelse(predictions$library.predict>=0.5,1,0)
library(caret)
cm <- confusionMatrix(as.factor(conv.preds[,"SL.randomForest_All"]), as.factor(Y_test))
cm

pred = predict(sl_lasso, X_test, onlySL = T)



#Individual model



#Regression Tree
library(tree)

X_train = data.frame(X_train)
X_test = data.frame(X_test)

par(mfrow=c(1,1))

tree.ksdf = tree(formula = Y_train ~ ., data = X_train)
summary(tree.ksdf)
plot(tree.ksdf)
text(tree.ksdf, pretty=0)

pred = predict(tree.ksdf, newdata=X_test)
pred = pred >= 0.5
pred = pred == Y_test
sum(pred)/281675


#Random Forest
attach(X_train)
library(randomForest)
bag.ksdf = randomForest(Y_train~., data= X_train, mtry=13, ntree=500, importance=TRUE)
plot(bag.ksdf)
summary(bag.ksdf)
importance(bag.ksdf)
pred_bag = predict(bag.ksdf, newdata=X_test)
pred_bag = pred_bag >= 0.5
pred_bag = pred_bag == Y_test
sum(pred_bag)/281675

#Boosting
library(gbm)
boost.ksdf = gbm(Y_train~., data= X_train, distribution="gaussian", n.trees=1000, interaction.depth=4)
summary(boost.ksdf)

par(mfrow=c(2,2))
plot(boost.ksdf,i="success_ratio")
plot(boost.ksdf,i="usd_goal_real")
plot(boost.ksdf,i="duration")
plot(boost.ksdf,i="country")
plot(boost.ksdf, i="main_category")
pred = predict(boost.ksdf, newdata=X_test, n.trees = 1000)
pred = pred >= 0.5
pred = pred == Y_test
sum(pred)/281675