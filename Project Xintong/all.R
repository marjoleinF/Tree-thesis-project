args <- commandArgs(TRUE) #SLURM command
args <- as.numeric(args)

#for example
args<-c(1,1)

startindex <- args[1]

replication <- args[2] #fixed number of replications per data set

set <- 250/replication
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
source('benchmarkmseED.R')
start<-1000+startinitial
#i='Servo'
for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
  for (j in c(3,4,5)) {
    #assign(paste('result',i,j,sep = '_'),mse_function(startinitial,get(i),replication,j))
    assign(paste('times',i,j,sep = '_'),mse_function(startinitial,get(i),replication,j))
    #now_result=get(paste('result',i,j,sep = '_'))
    #save(now_result, file =paste('result',i,j, "Start", start, ".RData",sep='')) 
  }
}
#result_Bostonhousing_3<-mse_function(startinitial,BostonHousing,replication,3)
#result_Bostonhousing_6<-mse_function(startinitial,BostonHousing,replication,6)
#result_Servo_3<-mse_function(startinitial,Servo,replication,3)
#result_Servo_6<-mse_function(startinitial,Servo,replication,6)

#save(result_Bostonhousing_3, file =paste("result_Bostonhousing_3", "Start", startinitial, ".RData",sep='')) 
#save(result_Servo_3, file =paste("result_Servo_3", "Start", startinitial, ".RData",sep='')) 

#replication,maxdepth