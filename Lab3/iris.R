library(rpart)
library(rpart.plot)

iris
dim(iris)

s_iris <- sample(150, 100)

iris_train <- iris[s_iris,]
iris_test <- -iris[-s_iris,]

dim(iris_test)
dim(iris_train)

decisionTreeModel <- rpart(Species~., iris_train, method='class')
decisionTreeModel

rpart.plot(decisionTreeModel)

summary(iris$Sepal.Length)
summary(iris$Petal.Length)
summary(iris$Petal.Width)
