library(MASS)
library(ggplot2)
titanicDS = read.csv("titanic.csv")
dim(titanicDS)

str(titanicDS)

summary(titanicDS)

attach(titanicDS)
UniqueValue = function (x) {length(unique(x)) }
apply(titanicDS, 2, UniqueValue)

NaValue = function (x) {sum(is.na(x)) }
apply(titanicDS, 2, NaValue)

BlankValue = function (x) {sum(x=="") }
apply(titanicDS, 2, BlankValue)

MissPercentage = function (x) {100 * sum (is.na(x)) / length (x) }
apply(titanicDS, 2, MissPercentage)

titanicDS$Age[is.na(titanicDS$Age)] = mean(titanicDS$Age, na.rm=TRUE)
apply(titanicDS, 2, MissPercentage)

set.seed(1)
row.number = sample(1:nrow(titanicDS), 0.6*nrow(titanicDS))
train = titanicDS[row.number,]
test = titanicDS[-row.number,]
dim(train)
dim(test)

# QDA model

attach(train)
qda.model = qda (factor(Survived)~factor(Pclass)+Sex+Age+SibSp, data=train)
qda.model

##Predicting training results.
predmodel.train.qda = predict(qda.model, data=train)
table(Predicted=predmodel.train.qda$class, Survived=Survived)

##Predicting test results.
attach(test)
predmodel.test.qda = predict(qda.model, newdata=test)
table(Predicted=predmodel.test.qda$class, Survived=test$Survived)

par(mfrow=c(1,1))
plot(predmodel.test.qda$posterior[,2], predmodel.test.qda$class, col=test$Survived+10)
