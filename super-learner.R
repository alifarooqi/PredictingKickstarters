install.packages("SuperLearner")
install.packages(c("caret", "glmnet", "randomForest", "ggplot2", "RhpcBLASctl"))

ksdf = read.csv("D:\\Projects\\PredictingKickstarters\\cleaned-ks-data.csv", header = TRUE)
data_without_live = subset(ksdf, ksdf$state != 'live')
data_without_live$state = factor(data_without_live$state)

state01 = rep(0, length(data_without_live$state))
state01[data_without_live$state == 'successful'] = 1
data = data.frame(data_without_live, state01)
predict_variable = data$state01
data = subset(data, select = c(-state, -state01))

train_obs = sample(nrow(data), 200)
X_train = data[train_obs, ]
X_test = data[-train_obs, ]
Y_train = predict_variable[train_obs]
Y_test = predict_variable[-train_obs]
table(Y_train, useNA = "ifany")
library(SuperLearner)

data = subset(data, select = c(-name, -ID, -X, -deadline, -launched, -pledged, -backers, -usd_pledged_real, -currency))

install.packages('fastDummies')
library(fastDummies)

data = dummy_cols(data)
data = subset(data, select = c(-country, -category, -main_category))

data$country = as.numeric(data$country)
data$category = as.numeric(data$category)
data$main_category = as.numeric(data$main_category)

sl_rf = sl_rf = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                             SL.library = "SL.randomForest")

sl_lasso = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                        SL.library = "SL.glmnet")


sl_3 = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                                    SL.library = c("SL.mean", "SL.glmnet", "SL.randomForest"))
sl_3


pred = predict(sl_rf, X_test, onlySL = T)
