data(iris)
mydata <- cbind(iris[5],iris[1:4]) 

library(e1071)
mysvm <- svm(Species ~ ., iris)
mysvm.pred <- predict(mysvm, iris)
table(mysvm.pred,iris$Species)
# mysvm.pred   setosa versicolor virginica
#   setosa     50      0          0
#   versicolor  0     48          2
#   virginica   0      2         48

#install.packages("klaR")

library(klaR)
mynb <- NaiveBayes(Species ~ ., iris)
mynb.pred <- predict(mynb,iris)
table(mynb.pred$class,iris$Species)
#              setosa versicolor virginica
#   setosa     50      0          0
#   versicolor  0     47          3
#   virginica   0      3         47

library(nnet)
mynnet <- nnet(Species ~ ., iris, size=1)
mynnet.pred <- predict(mynnet,iris,type="class")
table(mynnet.pred,iris$Species)
# mynnet.pred  setosa versicolor virginica
#   setosa     50      0          0
#   versicolor  0     49          1
#   virginica   0      1         49

library(MASS)
data(iris)


#Decision trees
library(rpart)
mytree <- rpart(Species ~ ., iris)
plot(mytree); text(mytree) # in "iris_tree.ps"
summary(mytree)
mytree.pred <- predict(mytree,iris,type="class")
table(mytree.pred,iris$Species)
# mytree.pred  setosa versicolor virginica
#   setosa     50      0          0
#   versicolor  0     47          1
#   virginica   0      3         49

# Leave-1-Out Cross Validation (LOOCV)
ans <- numeric(length(iris[,1]))
for (i in 1:length(iris[,1])) {
  mytree <- rpart(Species ~ ., iris[-i,])
  mytree.pred <- predict(mytree,iris[i,],type="class")
  ans[i] <- mytree.pred
}
ans <- factor(ans,labels=levels(iris$Species))
table(ans,iris$Species)
# The same as above in this case


#Quadratic Discriminant Analysis
library(MASS)
myqda <- qda(Species ~ ., iris)
myqda.pred <- predict(myqda, iris)
table(myqda.pred$class,iris$Species)
#              setosa versicolor virginica
#   setosa     50      0          0
#   versicolor  0     48          1
#   virginica   0      2         49


#Regularised Discriminant Analysis
library(klaR)
myrda <- rda(Species ~ ., iris)
myrda.pred <- predict(myrda, iris)
table(myrda.pred$class,iris$Species)
#              setosa versicolor virginica
#   setosa     50      0          0
#   versicolor  0     48          0
#   virginica   0      2         50

#Random Forests
library(randomForest)
myrf <- randomForest(Species ~ .,iris)
myrf.pred <- predict(myrf, iris)
table(myrf.pred, iris$Species)
# myrf.pred    setosa versicolor virginica
#   setosa     50      0          0
#   versicolor  0     50          0
#   virginica   0      0         50
# (Suspiciously correct! - need to read the manual)
