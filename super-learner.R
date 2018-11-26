install.packages("SuperLearner")
install.packages(c("caret", "glmnet", "randomForest", "ggplot2", "RhpcBLASctl"))

ksdf = read.csv("cleaned-ks-data.csv", header = TRUE)
data_without_live = subset(ksdf, ksdf$state != 'live')
data_without_live$state = factor(data_without_live$state)

state01 = rep(0, length(data_without_live$state))
state01[data_without_live$state == 'successful'] = 1
data = data.frame(data_without_live, state01)
predict_variable = data$state01
data = subset(data, select = c(-state, -state01))

library(SuperLearner)
library(fastDummies)

data = subset(data, select = c(-name, -ID, -X, -deadline, -pledged, -backers, -usd_pledged_real, -currency))
data$launched = substr(data$launched, 1,4)
data$success_ratio = rep(0, nrow(data))


################################################


for (i in 1:nrow(data)){
  data$success_ratio[i] = trend[data$main_category[i],data$launched[i]]
}
  


################################################

write.csv(data, file = "cleaned-ks-data-more.csv")



data = dummy_cols(data, select_columns = c(country, main_category, category, launched))
data = subset(data, select = c(-country, -category, -main_category, -launched))

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
