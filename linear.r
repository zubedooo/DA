setwd("C:\\Users\\Dhanushguntha\\Desktop\\dalabpractice")
lr<-read.csv("lr.csv",header=TRUE,sep=",")
lr
budget <-mean(lr$budget)
sales <- mean(lr$sales)
num<-0
den<-0

for(i in 1:nrow(lr)){
  num =num + (lr[i,2] - budget) * (lr[i,3] - sales)
  den = den + ((lr[i,2] - budget) ^ 2)
  # print(i)
}

num
den
b1<- num/den
b0<- sales - b1 * budget
b0
b1


lr$pred1<-b0 + b1 * lr$budget
k <- lr$pred1

lr

rss <- 0
for (i in 1:nrow(lr)){
  rss=rss+(lr[i,3]-k[i])^2
  #print(rss)
}

rse <- sqrt(rss/(nrow(lr)-2))    #rse=rss/(n-p-1) p=no. of predictors=1 n-1-1=n-2
rse

tss<- 0
for (i in 1:nrow(lr)){
  tss=tss+(lr[i,3]-sales)^2
  #print(tss)
}
tss

r_sqr <- 1 - rss / tss
r_sqr
x<- readline(prompt="Enter Budget: ")
# convert character into integer
x <- as.integer(x)
y <- b0 + b1 * x
y

#using predefined function
fit<-lm(sales~budget,lr)
summary(fit)
lr$pred2<-predict(fit,data.frame(budget=c(lr$budget)))
lr

plot(lr$pred2,lr$sales,xlab="predicted",ylab="actual")
abline(a=0,b=1)

lr

#plot predictor vs response values
plot(lr$budget,lr$sales,xlab="budget",ylab="sales")
abline(a=119.4331,b=0.02204)
