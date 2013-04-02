
# does die10 show a bigger value than die6?

rm(list=ls())
die6<- c(1:6, rep(1/6,6))
die10<-c(1:10,rep(1/10,10))
dim(die6)<-c(6,2)
dim(die10)<-c(10,2)

res<-rep(NA,120)
dim(res)<-c(6,10,2)

test<-rep(NA,60)
dim(test)<-c(6,10)

for (i in 1:6){
  for (j in 1:10){
    res[i,j,1]<-die6[i,2]*die10[j,2]
    res[i,j,2]<-die10[j,1]>die6[i,1]
  }
}

a<-sum(res[,,1][res[,,2]==1])
a
#a is then the prob that the die shows a larger value on die 10 than die 6 = 0.65


#The fair coin? It was flipped 250 times and 140 were heads and 110 were tails

rm(list=ls())
data<-c(rep(1,140), rep(0,110))
prior.tmp<-rep(NA,30000)
dim(prior.tmp)<-c(10000,3)
prior.tmp[,1]<-runif(10000)
prior.tmp[,2]<-1

for (i in 1:length(data)){
  prior.tmp[,2]<-abs((1-data[i])-prior.tmp[,1])*prior.tmp[,2]
  }
plot(prior.tmp[,1], prior.tmp[,2])