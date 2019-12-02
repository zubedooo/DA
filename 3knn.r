bank<-read.csv("loan.csv",header=TRUE,sep=",")
bank
#x<- readline(prompt="Enter Age: ")
#x <- as.integer(x)
#y <- readline(prompt="Enter Loan Amount: ")
#y <- as.integer(y)
x=100
y=2000

print(typeof(bank$age))
dis <- transform(bank, distance= sqrt((x-bank$age)^2+(y-bank$loan)^2 ) )
dis

odis<- dis[order(dis$distance),c(5,6)]
odis

k<- readline(prompt="Enter k: ")
# convert character into integer

k <- as.integer(k)
nn<-head(odis,k)
knn<-table(nn$paid)
nn
knn
t<-names(which(table(nn$paid)==max(table(nn$paid))))  # finding which class has got max occurences
t
cat("class:",t[[1]][1])



