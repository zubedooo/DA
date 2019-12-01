setwd("C:\\Users\\Dhanushguntha\\Desktop\\dalabpractice")
lr<-read.csv("p5.csv",header=TRUE,sep=",")

lr<-lr[1:10,]
lr
tv <-mean(lr$TV)
rad <- mean(lr$radio)
sale <- mean(lr$sales)
tv
rad
sale

#TV
num<-0
den<-0
for(i in 1:nrow(lr)){
  num =num + (lr[i,1] - tv) * (lr[i,4] - sale)
  den = den + ((lr[i,1] - tv) ^ 2)
  # print(i)
}

num
den
b1<- num/den
b1
summary(lm(sales~TV,data=lr))

#radio
num<-0
den<-0

for(i in 1:nrow(lr)){
  num =num + (lr[i,2] - rad) * (lr[i,4] - sale)
  den = den + ((lr[i,2] - rad) ^ 2)
  # print(i)
}
num
den
b2<- num/den
b2
summary(lm(sales~radio,data=lr))

#b0
b0<-sale-(b1*tv )-(b2*rad)
b0
summary(lm(sales~radio,data=lr))

lr$pred1<-b0 + ( b1 * lr$TV)  + ( b2  * lr$radio )
k<-lr$pred1
rss <- 0
for (i in 1:nrow(lr)){
  rss=rss+(lr[i,4]-k[i])^2
  #print(rss)
}
rss

rse <- sqrt(rss/(nrow(lr)-3))     #p=2 n-2-1=n-3
rse

tss<- 0
for (i in 1:nrow(lr)){
  tss=tss+(lr[i,4]-sale)^2
  #print(tss)
}
tss

r_sqr <- 1 - rss / tss
r_sqr


x1<- readline(prompt="Enter TV Budget: ")
# convert character into integer
x1 <- as.integer(x1)
x1

x2<- readline(prompt="Enter Radio Budget: ")
x2 <- as.integer(x2)
x2

y <- b0 + b1 * x1 + b2 * x2
y


mod1<-lm(sales~TV+radio+newspaper,data=lr)
summary(mod1)
lr$pred2<-predict(mod1,data.frame(TV=c(lr$TV),radio=c(lr$radio),newspaper=c(lr$newspaper)))
lr
#lr$pred2=predict(mod1,data.frame(TV=200,radio=20,newspaper=30))
#lr
plot(lr$pred1,lr$sales,xlab="predicted",ylab="actual")
abline(a=0,b=1)
plot(lr$pred2,lr$sales,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)
b1
b2
lr

