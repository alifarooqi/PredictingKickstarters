library(fastDummies)

ksdf = read.csv("cleaned-ks-data-more.csv", header = TRUE)
data = dummy_cols(ksdf, select_columns = c("category", "launched"))
data = subset(data, select = c(-category, -launched, -X.1, -X, -success_ratio))
train_obs = sample(nrow(data), 30000)
X_train = data[train_obs, ]
X_test = data[-train_obs, ]
Y_train = data[train_obs, 'state01']
Y_test = data[-train_obs, 'state01']
glm.fits = glm(state01~., data = data, subset=train_obs, family = binomial)
glm.probs = predict(glm.fits,X_test, type='response')
glm.pred = rep(0, nrow(X_test))
glm.pred[glm.probs>0.5] = 1
table(glm.pred, X_test[,'state01'])
library(leaps)
regfit.full = regsubsets(state01???.,X_train,nvmax=10, really.big=T)
summary(regfit.full)

library(tree)

X_train = data.frame(X_train)
X_test = data.frame(X_test)

tree.ksdf = tree(formula = Y_train ~ .-state01, data = X_train)
summary(tree.ksdf)
plot(tree.ksdf)
text(tree.ksdf, pretty=0)

pred = predict(tree.ksdf, newdata=X_test)
pred = pred >= 0.5
pred = pred == Y_test
sum(pred)/281675
