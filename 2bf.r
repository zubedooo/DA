b1<-c(1,2,1,1)
b2<-c(9,10,9,9)
b3<-c(4,2,4,3)
b4<-c(10,9,10,10)
id <-c(1:4)
df <- data.frame(id,b1,b2,b3,b4)

for(i in 1:nrow(df)){
  pie(c(df$b1[i],df$b2[i],df$b3[i],df$b4[i]), c("b1","b2","b3","b4"))
}
b1_avg = mean(df$b1)
b2_avg = mean(df$b2)
b3_avg = mean(df$b3)
b4_avg = mean(df$b4)
pie(c(b1_avg,b2_avg,b3_avg,b4_avg),c("b1","b2","b3","b4"))
