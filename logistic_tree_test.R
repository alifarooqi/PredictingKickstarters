data = read.csv("ks-projects-201801.csv")
data = data[data$state == 'successful' | data$state == 'failed', ]
data$state = factor(data$state)
train = sample(nrow(data), 10000)
glm.fits=glm(state???usd_goal_real+category, data=data , family = binomial, subset = train)
test_data = data[-train,]
glm.fits$xlevels[["category"]] <- union(glm.fits$xlevels[["category"]], levels(test_data$category))
glm.probs = predict(glm.fits,test_data, type='response')
glm.pred = rep("Failed", nrow(test_data))
glm.pred[glm.probs>0.5] = 'successful'
table(glm.pred, test_data[,'state'])
library(tree)
tree.state = tree(state~main_category+usd_goal_real , data )
plot(tree.state)
text(tree.state, pretty=0)