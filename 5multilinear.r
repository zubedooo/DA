df=read.csv('lr.csv')

#Y=m0 + m1x1+m2x2...

mean_tv=mean(df$TV)
mean_sales=mean(df$Sales)
m1=sum((df$TV-mean_tv)*(df$Sales-mean_sales))/sum((df$TV-mean_tv)**2)

m1
mean_radio=mean(df$Radio)
m2=sum((df$Radio-mean_radio)*(df$Sales-mean_sales))/sum((df$Radio-mean_radio)**2)
m0=mean_sales-m1*mean_tv-m2*mean_radio
m2
m0
df$pred=m0+m1*df$TV+m2*df$Radio

#prebuilt
m0
m1
m2
lin_model=lm(Sales~TV+Radio,data=df)
lin_model
