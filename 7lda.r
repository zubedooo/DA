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

# LDA model
attach(train)
lda.model = lda (factor(Survived)~factor(Pclass)+Sex+Age+SibSp, data=train)
lda.model


##Predicting training results.
predmodel.train.lda = predict(lda.model, data=train)
table(Predicted=predmodel.train.lda$class, Survived=Survived)

ldahist(predmodel.train.lda$x[,1], g= predmodel.train.lda$class)


attach(test)
predmodel.test.lda = predict(lda.model, newdata=test)
table(Predicted=predmodel.test.lda$class, Survived=test$Survived)

par(mfrow=c(1,1))
plot(predmodel.test.lda$x[,1], predmodel.test.lda$class, col=test$Survived+10)
