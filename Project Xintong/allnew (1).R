args <- commandArgs(TRUE) #SLURM command
args <- as.numeric(args)

#for example
args<-c(1,2)

startindex <- args[1]

replication <- args[2] #fixed number of replications per data set

set <- 100/replication
vec <- 1
for(i in 1:(set-1)){
  vec <- c(vec, i*replication+1)
}

startinitial <- vec[startindex]



library(mlbench)
library(rpart)
library(evtree)
library(dbarts)
library(ISLR)
library(MASS)
library(MAVE)



data('BostonHousing')
data('Servo')
data('Hitters')
#data('Ozone')
#data('College')
data('Concrete')
data('Auto')
data('Wage')
data('cpus')

Auto<-Auto[,c(2:8,1)]
Hitters<-Hitters[!(is.na(Hitters$Salary)),]
rownames(Hitters)<-1:nrow(Hitters)
Hitters<-Hitters[,c(names(Hitters)[1:18],names(Hitters)[20],names(Hitters)[19])]
Wage<-Wage[,c(1:9,11)]
cpus<-cpus[,2:8]
names(Concrete)<-paste0('V',1:9)
source('benchmarkmseED (1).R')

start<-1000+startinitial

for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
  
    setwd("/home/emldusseldorp/Xintong")  
    j<-3
    now_result3 <- mse_function(startinitial,get(i),replication,j)
    j<-4
    now_result4 <- mse_function(startinitial,get(i),replication,j)
    j<-5
    now_result5 <- mse_function(startinitial,get(i),replication,j)
    
    setwd("/exports/fsw/emldusseldorp/Xintong")
    j<-3
    save(now_result3, file =paste('result',i,j, "Start", start, ".RData",sep='')) 
    j<-4
    save(now_result4, file =paste('result',i,j, "Start", start, ".RData",sep='')) 
    j<-5
    save(now_result5, file =paste('result',i,j, "Start", start, ".RData",sep='')) 
}


