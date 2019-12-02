df=read.csv('lr.csv')
df_new=data.frame(df$TV,df$Sales)
colnames(df_new)=c("TV","Sales")
mean_tv=mean(df_new$TV)
mean_sales=mean(df_new$Sales)
m1=sum((df$TV-mean_tv)*(df$Sales-mean_sales))/sum((df$TV-mean_tv)**2)
m0=mean_sales-m1*mean_tv
df_new$pred=m0+m1*df_new$TV
plot(df$TV,df$Sales,col="red")
lines(df$TV,df_new$pred,col="blue")

#Using prebuilt function

lin_model=lm(Sales~TV,data=df_new)
new=data.frame(TV=df$TV)
pred=predict(lin_model,new)
plot(df$TV,df$Sales)
lines(df$TV,pred)
