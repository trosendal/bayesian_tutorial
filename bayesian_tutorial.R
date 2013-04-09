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
sum(posterior[,3][(posterior[,1]>0.4979908)&(posterior[,1]<0.620111)])
#The most likely value is the centre of max of the posterior distribution and
#it is calculated as follows:
mlv<-sum(posterior[,1]*posterior[,3])
#This means that the most likely probability heads for this coin given the
#observed evidence is 0.5595 which is equivalent to the proportion of heads in
#the evidence because the pror distribution we started with was a uniform

#The 95% credibility interval of the point estimate for the coind is 
#the range of of probabilities on the x axis that include 95% of the are under the
#curve because we have probability summing to 1 we know that we are looking for
#the point of 0.025 area on the right and 0.025 area on the left

central_posterior <- function(posterior, ll=0.025, cnt=0.5, ul=0.975) {
  if(!all(ll > 0, ll < 1, ul > ll, ul < 1))
    stop("Out of range")
  a<-cumsum(posterior[,3])
  b<-(posterior[c(max(which(a < ll)),max(which(a < cnt)), min(which(a > ul)) ),1])
  b<-c("lower95%", "Median", "Upper95%", b)
  dim(b)<-c(3,2)
  return(b)
  #return(class(b))
}

central_posterior(posterior)

# is Alice smarter?
#Alice gets 780 on the SAT and bob gets 760
#we have a prior distribution of SAT scores from the population
#Then we ask: Is alice smarter than bob. ie is alice more likely to 
#correctly answer a question than bob


#First the distributions of the  rest of the students that took the test. The prior

library("stringr")

temp_a<-(read.table("H:/bayesian_tutorial/sat_ranks.csv", sep=",", colClasses = "character"))[4:64,1:2]
prior_a<-as.numeric(c(temp_a$V1, temp_a$V2))
dim(prior_a)<-c(61,2)
prior_a<-as.data.frame(prior_a)
plot(prior_a[,1], prior_a[,2])

matchscorelist<-(read.table("H:/bayesian_tutorial/sat_scale.csv", sep=",", colClasses = "character"))[6:18, 3:4]
matchscorelist
c.tmp<-(str_split(matchscorelist$V4, "-"))
d.tmp<-sapply(c.tmp, function(item){
  as.numeric(item[1])  
})
e.tmp<-sapply(c.tmp, function(item){
  as.numeric(item[2])  
})
f.tmp<-(d.tmp+e.tmp)/2
f.tmp[1]<-d.tmp[1]
f.tmp[13]<-d.tmp[13]
matchscorelist$V4<-f.tmp
matchscorelist$V3<-as.numeric(matchscorelist$V3)

plot(glm(matchscorelist$V3~matchscorelist$V4)$fitted.values, matchscorelist$V4, type="l")
a.tmp<-((0:60)+20)*10
b.tmp<-round(((glm(matchscorelist$V3~matchscorelist$V4)$coefficients)[1])+((glm(matchscorelist$V3~matchscorelist$V4)$coefficients)[2])*a.tmp)
prior_a$V3<-rev(b.tmp)
names(prior_a)<-c("score", "freq", "num_correct")

plot(prior_a$num_correct, prior_a$freq)

#Alice got 780 and Bob got 760, we now know from the prior_a object that this corresponds to Bob 
#answering 53/57 correct and Alice 55/57. These are then the values we will use to update the prior



