#mse_evtree<-apply((t(result_H_6$result_mat_evtree[,1:(ncol(result_H_6$result_mat_evtree)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(result_H_6$result_mat_evtree_train[,1:(ncol(result_H_6$result_mat_evtree_train)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368
#mse_rpart<-apply((t(result_H_6$result_mat_rpart[,1:(ncol(result_H_6$result_mat_rpart)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(result_H_6$result_mat_rpart_train[,1:(ncol(result_H_6$result_mat_rpart_train)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368
#mse_bart<-apply((t(result_H_6$result_mat_bart[,1:(ncol(result_H_6$result_mat_bart)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(result_H_6$result_mat_bart_train[,1:(ncol(result_H_6$result_mat_bart_train)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368
#mse_born<-apply((t(result_H_6$result_mat_born[,1:(ncol(result_H_6$result_mat_born)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(result_H_6$result_mat_born_train[,1:(ncol(result_H_6$result_mat_born_train)-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368
# compute MSE
mse_rpart<-apply((t(result_mat_rpart)-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(result_mat_rpart_train)-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368
mse_bart<-apply((t(result_mat_bart)-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(result_mat_bart_train)-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368
mse_born<-apply((t(result_mat_born)-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(result_mat_born_train)-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368
library(mlbench)
library(rpart)
library(evtree)
library(dbarts)
library(ISLR)
library(MASS)
library(MAVE)
library(gridExtra)


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
#names(Concrete)<-paste0('V',1:9)


for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
  dataset=get(i)
  for (j in c(3,4,5)) {
    list_name<-paste0(i,j)
    list_name<-get(list_name)
    n=0
    for (h in c('rpart','bart','born','evtree')) {
      n=n+1
      mse_name<-paste(i,j,h,sep='_')
      if(n==1){
      node_name<-paste(i,j,h,'node',sep='_')
      assign(node_name,list_name[[n+8]])
      }
      else{if(n!=2){
      node_name<-paste(i,j,h,'node',sep='_')
      assign(node_name,list_name[[n+7]])}}
      if(n==2){
        node_name<-paste(i,j,h,'node',sep='_')
        assign(node_name,rep(0,length(list_name[[n+7]])))}
      assign(mse_name,apply((t(list_name[[2*n-1]][,1:(ncol(list_name[[2*n-1]])-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.632+apply((t(list_name[[2*n]][,1:(ncol(list_name[[2*n]])-1)])-dataset[,ncol(dataset)])^2,2,mean,na.rm = T)*0.368)
    }
  }
}
# compute relative MSE
for (i in c(3,4,5)) {
  d_name<-paste('dep',i,sep='_')
  node_name<-paste('node',i,sep='_')
  n_node<-data.frame(matrix(nrow = 250*4,ncol = 9))
  data_mse<-data.frame(matrix(nrow = 250*4,ncol = 9))
  n=1
  for (j in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
    for (h in c('rpart','born','evtree','bart')) {
        now_data_name<-paste(j,i,h,sep = '_')
        now_bart<-paste(j,i,'bart',sep = '_')
        now_rpart<-paste(j,i,'rpart',sep = '_')
        now_evtree<-paste(j,i,'evtree',sep = '_')
        now_born<-paste(j,i,'born',sep = '_')
        now_node<-paste(j,i,h,'node',sep = '_')
        now_node_rpart<-paste(j,i,'rpart','node',sep = '_')
        now_node_evtree<-paste(j,i,'evtree','node',sep = '_')
        now_node<-get(now_node)
        now_node_rpart<-get(now_node_rpart)
        now_node_evtree<-get(now_node_evtree)
        now_bart<-get(now_bart)
        now_born<-get(now_born)
        now_evtree<-get(now_evtree)
        now_rpart<-get(now_rpart)
        now_data<-get(now_data_name)
        now_data_name<-paste(j,h,sep = '_')
        data_mse[n:(n+249),1]<-(now_data-now_bart)/now_bart
        data_mse[n:(n+249),2]<-now_data_name
        data_mse[n:(n+249),3]<-h
        data_mse[n:(n+249),4]<-now_node
        
        
        data_mse[n:(n+249),5]<-(now_data-now_rpart)/now_rpart
        data_mse[n:(n+249),6]<-(now_data-now_evtree)/now_evtree
        data_mse[n:(n+249),7]<-(now_node-now_node_rpart)/now_node_rpart
        data_mse[n:(n+249),8]<-(now_node-now_node_evtree)/now_node_evtree
        data_mse[n:(n+249),9]<-j
        #n_node[n:(n+249),1]<-now_node
        #n_node[n:(n+249),2]<-now_data_name
        #n_node[n:(n+249),3]<-h
        n=n+250
          }
    
  }
  assign(d_name,data_mse)
  assign(node_name,n_node)
}
#get best depth
for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
  for (h in c('rpart','born','evtree')) {
    best_depth=0
    mse=9999999
    for (j in c(3,4,5)){
      list_name<-paste(i,j,h,sep = '_')
      dataset<-get(list_name)
      if(mean(dataset)<mse){
        mse<-mean(dataset)
        best_depth<-j
      }
    }
    print(paste(i,best_depth,h,sep = '_'))
  }
}

# for compare means of different depth
tables<-data.frame(matrix(nrow=54,ncol=4))
names(tables)<-c('dataset','depth','method','mean_squared_error')
n=0
for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
  for (h in c('rpart','born','evtree')) {

    for (j in c(3,4,5)){
      n<-n+1
      list_name<-paste(i,j,h,sep = '_')
      list_name<-get(list_name)
      tables[n,1:3]<-c(i,j,h)
      tables[n,4]<-mean(list_name)
      
    }
  }
}
function_bar(tables[tables$dataset=='BostonHousing',],method,mean_squared_error,depth
             ,'')
#minn<-mean(0.3*tables[tables$dataset=='cpus',4])
a<-ggplot(tables[tables$dataset=='cpus',], aes(x = method,y = mean_squared_error,fill = depth))+
  geom_bar(stat ="identity",width = 0.7,position = "dodge") +
  labs(x = "",y = "MSE", title = 'cpus')
b<-ggplot(tables[tables$dataset=='Hitters',], aes(x = method,y = mean_squared_error,fill = depth))+
  geom_bar(stat ="identity",width = 0.7,position = "dodge")  +
  labs(x = "",y = "MSE", title = 'Hitters')  
c<-ggplot(tables[tables$dataset=='Servo',], aes(x = method,y = mean_squared_error,fill = depth))+
  geom_bar(stat ="identity",width = 0.7,position = "dodge")  +
  labs(x = "",y = "MSE", title = 'Servo')  
d<-ggplot(tables[tables$dataset=='Auto',], aes(x = method,y = mean_squared_error,fill = depth))+
  geom_bar(stat ="identity",width = 0.7,position = "dodge")  +
  labs(x = "",y = "MSE", title = 'Auto')  
e<-ggplot(tables[tables$dataset=='Concrete',], aes(x = method,y = mean_squared_error,fill = depth))+
  geom_bar(stat ="identity",width = 0.7,position = "dodge")  +
  labs(x = "",y = "MSE", title = 'Concrete')  
f<-ggplot(tables[tables$dataset=='BostonHousing',], aes(x = method,y = mean_squared_error,fill = depth))+
  geom_bar(stat ="identity",width = 0.7,position = "dodge")  +
  labs(x = "",y = "MSE", title = 'BostonHousing')  


grid.arrange(arrangeGrob(a,b,c,d,e,f,ncol = 3))
  