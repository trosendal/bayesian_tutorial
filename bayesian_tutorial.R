
# Question 1: does die10 show a bigger value than die6?

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


#Question 2: Is the coin fair? It was flipped 250 times and 140 were heads and 110 were tails

rm(list=ls())
data<-c(rep(1,140), rep(0,110))
prior.tmp<-rep(NA,30000)
dim(prior.tmp)<-c(10000,3)

#Option 1 A uniform prior
#prior.tmp[,1]<-runif(10000)

#Option 2 A uniform prior build by a sequence of equally spaced numbers
#this is better than the uniform distribution because of the pure
#uniformity of the numbers
prior.tmp[,1]<-seq(from=0.01,to=0.99, length=10000)

#option 3 see what happens if you start with a normal prior
#prior.tmp[,1]<-rnorm(10000, mean=0.5, sd=0.1)

prior.tmp[,2]<-1

for (i in 1:length(data)){
  prior.tmp[,2]<-abs((1-data[i])-prior.tmp[,1])*prior.tmp[,2]
  }
#now normalize by deviding by the total amount of remaining confidence
prior.tmp[,3]<-prior.tmp[,2]/sum(prior.tmp[,2])
#And plot the confidence by hypothesis for the posterior distribution
plot(prior.tmp[,1], prior.tmp[,3])
posterior<-prior.tmp
#note that the scale of the y axis is 'relative confidnece'
#it is unitless and related to the number of initial hypotheses in the
#prior distribution.

#This is the sum of all likelihoods
sum(posterior[,3])
#This is the sum in a small range around 0.5
sum(posterior[,3][(posterior[,1]>0.4)&(posterior[,1]<0.6)])
#The most likely value is the centre of max of the posterior distribution and
#it is calculated as follows:
sum(posterior[,1]*posterior[,3])
#This means that the most likely probability heads for this coin given the
#observed evidence is 0.5595 which is equivalent to the proportion of heads in
#the evidence because the pror distribution we started with was a uniform

#The 95% credibility interval of the point estimate for the coind is 
#the range of of probabilities on the x axis that include 95% of the are under the
#curve because we have probability summing to 1 we know that we are looking for
#the point of 0.025 area on the right and 0.025 area on the left

posterior[,2]<-seq(from=1,to=10000, length=10000)

for (i in 1:10000) {
  a<-sum(posterior[,3][(posterior[,2]<=i)])
  
}












# is Alice smarter?
#Alice gets 780 on the SAT and bob gets 760
#we have a prior distribution of SAT scores from the population
#Then we ask: Is alice smarter than bob. ie is alice more likely to 
#correctly answer a question than bob

#Now we can reformulate the question and ask if the coin is biased in a different way
#the coin could be in a range. We don't know where in the range but
#could ask if it is in the range. Let's say we want to know if the liklihood of the 
#coin being anywhere between 0 and 1 is greater than the likelihood of the coin being 0.5
