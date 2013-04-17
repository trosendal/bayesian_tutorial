#To run a winbugs model from R you need R2WinBUGS, and winbugs (OR OpenBugs but then you also need the package BRugs)
#You can have the data and inits in 'named lists' in R and the model in a text file that you call from the bugs command
#you can also stay within R and write the model specification as a function and then create the write.model command
#in the R2WinBugs package to pass it on to the Bugs function. 

#MODEL
#model a single variable binomial process. This might be a coin being flipped and you want to know 
#what the range of possible values is for the fraction of heads (theta) when the coin was flipped a
#number of times (n) and it turned up heads a number of times (s). Before we did the experiment we had
#no prior knowledge about the coin or any coins 

#There is a text file with model named 'test.odc' in "H:/bayesian_tutorial" that has the following:

#model{
#  s~dbin(theta, n)
#  theta~dunif(0,1)
#}

#The data is in the object named data_coin This line would typically be in the bugs code if you were running
#the model in winbugs/openbugs. The model does not have any initial values; winbugs generates those for us.
#Some of the specifications in the model above are actually the default for the 
#command "bugs" but I write them to be explicit about the definition of this 
#model. See the help for the bugs command to determine the specifications

require(R2WinBUGS); require(BRugs)

setwd("H:/bayesian_tutorial")
data_coin<-list(n=250, s=140)
a<-bugs(data_coin, inits=NULL, parameters.to.save=c("theta"), model.file="test.odc",
     n.chains=1, n.iter=1000000, n.burnin=25000,
     n.thin=1000,
     debug=FALSE, DIC = TRUE, digits=5, codaPkg=FALSE,
     bugs.directory="c:/Program Files/OpenBUGS/OpenBUGS322/",
     program="OpenBUGS",
     working.directory=NULL, clearWD=FALSE,
     useWINE=.Platform$OS.type != "windows", WINE=NULL,
     newWINE=TRUE, WINEPATH=NULL, bugs.seed=NULL, summary.only=FALSE,
     save.history=!summary.only, over.relax = FALSE)

#These are the objects that the model generates

a$DIC
a$DICbyR
a$dimension.short
a$indexes.short
a$isDIC
a$last.values
a$long.short
a$mean
a$median
a$model.file
a$n.burnin
a$n.chains
a$n.iter
a$n.keep
a$n.sims
a$n.thin
a$pD
a$program
a$root.short
a$sd
a$sims.array
a$sims.list
a$sims.matrix
a$summary

#and the posterior looks like this:

hist(a$sims.matrix[,1])
a$summary


#The we can run the same model again right from here without modifying an
#external text file so we can easily see the code in the r script

require(R2WinBUGS); require(BRugs)

#first define the model and put it in a temporary text file that the bugs command will use 

model.temp <- function(){
  s~dbin(theta, n)
  theta~dunif(0,1)
}

filename <- file.path(tempdir(), "temp_model.bug")
write.model(model.temp, filename)

#The data
data_coin<-list(n=250, s=140)

#Run the model, not that the path for the model text file is now written as filename (the object you jsut created)

a<-bugs(data_coin, inits=NULL, parameters.to.save=c("theta"), model.file=filename,
        n.chains=1, n.iter=1000000, n.burnin=25000,
        n.thin=1000,
        debug=FALSE, DIC = TRUE, digits=5, codaPkg=FALSE,
        bugs.directory="c:/Program Files/OpenBUGS/OpenBUGS322/",
        program="OpenBUGS",
        working.directory=NULL, clearWD=FALSE,
        useWINE=.Platform$OS.type != "windows", WINE=NULL,
        newWINE=TRUE, WINEPATH=NULL, bugs.seed=NULL, summary.only=FALSE,
        save.history=!summary.only, over.relax = FALSE)


hist(a$sims.matrix[,1])
a$summary
