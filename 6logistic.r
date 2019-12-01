setwd("C:\\Users\\Dhanushguntha\\Desktop\\dalabpractice")

lr=read.csv("sales.csv",header = TRUE,sep = ',')

lr


num=0
den=0
lr$sales_increase=gsub("Yes",1,lr$sales_increase)
lr$sales_increase=gsub("No",0,lr$sales_increase)
lr
x=mean(lr$budget)
y=mean(as.integer(lr$sales_increase))
x
y
lr
for(i in 1:nrow(lr))
{
  num=num+((lr[i,2]-x)*(as.integer(lr[i,1])-y))
  den=den+((lr[i,2]-x)^2)
}

b1=num/den
b1
b0=y-b1*x
b0

lr$logodds=b0+b1*lr$budget

e=2.71

lr$odds=e^lr$logodds

lr$odds

lr


lr$prob=lr$odds/(1+lr$odds)

lr

glm.fits=glm(as.integer(lr$sales_increase)~lr$budget,data=lr,family=binomial,control = list(maxit=5))

summary(glm.fits) 

predict=predict(glm.fits,data=df[1:7,],type="response")

predict

inc_dec <- ifelse(predict > 0.5,1,0)
inc_dec 


