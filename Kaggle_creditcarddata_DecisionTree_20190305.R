tree.data = read.csv("C:\\Users\\snarayanaswamy\\Downloads\\creditcard.csv", header = TRUE)

names(tree.data)

tree.data1 = subset(tree.data,select = -c(Time)) ## Method 1: Removing variables using subset function

install.packages("dplyr") ## Method 2: Removing variables using dplyr package
library(dplyr)
tree.data2 = select(tree.data,-Time)

variable.names(tree.data1) ## Printing variable names
variable.names(tree.data2) ## Printing variable names

## Converting integer variable into a class/factor
tree.data1$Class = as.factor(tree.data1$Class)
table = table(tree.data1$Class) ##checking frequencies of factor variable
table
prop.table(table)


install.packages("caret", dependencies=c("Depends", "Suggests"))
install.packages("caret")
library(caret)
set.seed(12345)
intrain = createDataPartition(y=tree.data1$Class, p=0.7,list=FALSE)

training.data=tree.data1[intrain, ] ## creating training data set
testing.data=tree.data1[-intrain, ] ## creating testing data set


table(training.data$Class)
prop.table(table(training.data$Class))
table(testing.data$Class)
prop.table(table(testing.data$Class))


modfit = train(Class ~., method = "rpart",data=training.data)
print(modfit$method)
print(modfit$modelInfo)
print(modfit$modelType)
print(modfit$results)
print(modfit$pred)
print(modfit$finalModel)

##plotting classification tree
plot(modfit$finalModel, uniform = TRUE, main="Classification Tree")
text(modfit$finalModel, use.n=TRUE, all=TRUE, cex = .8)

library(rattle)
fancyRpartPlot(modfit$finalModel)

prediction = predict(modfit, newdata=testing.data)

dat2 = cbind.data.frame(testing.data,prediction)
dat2
dat3 = select(dat2,Class,prediction)
dat3

str(dat2)
confusionMatrix(data=dat3, reference=Class)
?confusionMatrix
