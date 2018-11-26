install.packages("SuperLearner")
install.packages(c("caret", "glmnet", "randomForest", "ggplot2", "RhpcBLASctl"))

library(SuperLearner)
library(fastDummies)

ksdf_state = read.csv("cleaned-ks-data.csv", header = TRUE)
data_without_live = subset(ksdf_state, ksdf_state$state != 'live')
data_without_live$state = factor(data_without_live$state)

state01 = rep(0, length(data_without_live$state))
state01[data_without_live$state == 'successful'] = 1


ksdf = read.csv("cleaned-ks-data-more.csv", header = TRUE)
data = dummy_cols(ksdf, select_columns = c("country", "main_category", "category", "launched"))
data = subset(data, select = c(-country, -category, -main_category, -launched, -X.1, -X))


predict_variable = data$state01

data = subset(data, select = -state01)

train_obs = sample(nrow(data), 5000)
X_train = data[train_obs, ]
X_test = data[-train_obs, ]
Y_train = predict_variable[train_obs]
Y_test = predict_variable[-train_obs]


sl_rf = sl_rf = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                             SL.library = "SL.randomForest")

sl_lasso = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                        SL.library = "SL.glmnet")


sl_3 = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                                    SL.library = c("SL.mean", "SL.glmnet", "SL.randomForest"))
sl_3


pred = predict(sl_lasso, X_test, onlySL = T)
