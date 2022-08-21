smearing<-function(data)
{ 
  m<-sample(1:nrow(data),1)
  Xm<-data[m,]
  index<-sample(1:nrow(data),(ncol(data)-1),replace = T)
  #for (j in 1:(ncol(data)-1))
  j=sample(1:(ncol(data)-1),1)
  my<-rep(m,times=(ncol(data)-1))
  my[j]<-sample(c(index[j],m),1)
  Xm[j]<-data[my[j],j]
  
  return(Xm)
}

mse_function<-function(I,dataset,replication,maxd){
  #dataset=mlbench.friedman1(500)
  #dataset=as.data.frame(cbind(dataset[['x']],dataset[['y']]))

  #source('smearing.r')
  
  n <- 1:nrow(dataset)
  K <- replication #number of replications (first start with a low number)
  i<-1
  result_mat_evtree <- matrix(nrow = K,ncol =nrow(dataset)+1 )
  result_mat_rpart<-matrix(nrow = K,ncol =nrow(dataset)+1  )
  result_mat_bart<-matrix(nrow = K,ncol =nrow(dataset)+1  )
  result_mat_born<-matrix(nrow = K,ncol =nrow(dataset)+1  )
  
  
  result_mat_evtree_train <- matrix(nrow = K,ncol =nrow(dataset)+1 )
  result_mat_rpart_train<-matrix(nrow = K,ncol =nrow(dataset)+1 )
  result_mat_bart_train<-matrix(nrow = K,ncol =nrow(dataset)+1 )
  result_mat_born_train<-matrix(nrow = K,ncol =nrow(dataset)+1 )
  node_evtree<-numeric(K)
  node_rpart<-numeric(K)
  node_born<-numeric(K)
  time_rp<-numeric(K)
  time_ev<-numeric(K)
  time_bo<-numeric(K)
  
  for (k in I:(K+I-1)) {
    set.seed(k* 1000) # I agree to set the seed for each replication k, in this way it    is easy to reproduce your findings.
    index <- sample(n, replace = TRUE)
    train <- dataset[index,]
    train_index<-unique(index)
    test_index <- n[-train_index]
    test <- dataset[test_index,]
    #apply evtree: use hyperparameters that are comparable to the other methods
     #maxdepth of the tree is 3
    start=Sys.time()
    control_evtree <- evtree.control(maxdepth = maxd,minbucket = 10, minsplit = 20) 
    model_evtree<- evtree(as.formula(paste0(names(dataset)[length(names(dataset))],'~.')), data=train, control=control_evtree)
    result_mat_evtree[i,test_index] <- predict(model_evtree,test)
    result_mat_evtree_train[i,train_index] <- predict(model_evtree,dataset[train_index,])
    result_mat_evtree[i,ncol(result_mat_evtree)]<-k
    result_mat_evtree_train[i,ncol(result_mat_evtree)]<-k
    s=0
    for (j in 1:length(model_evtree)) {
      s=s+(length(model_evtree[j])==1)
    }
    node_evtree[i]<-s
    end<-Sys.time()
    time_ev[i]<-start-end
    ##also add rpart and bart here (without born again)
    #rpart
    start<-Sys.time()
    control2<-rpart.control(minbucket = 10, minsplit = 20,maxdepth=maxd, xval=0) 
    #Comment ED: do not perform crossvalidation (set xval = 0) and set minbucket to 10 otherwise it is set to 20/3 by default.
    model_rpart<-rpart(as.formula(paste0(names(dataset)[length(names(dataset))],'~.')), data=train,control = control2)
    result_mat_rpart[i,test_index] <- predict(model_rpart, newdata= test)
    result_mat_rpart_train[i,train_index] <- predict(model_rpart,dataset[train_index,])
    node_rpart[i]<-sum(model_rpart[["frame"]][["var"]]=='<leaf>')
    result_mat_rpart[i,ncol(result_mat_evtree)]<-k
    result_mat_rpart_train[i,ncol(result_mat_evtree)]<-k
    end<-Sys.time()
    time_rp[i]<-start-end
    #BART
    result<-bart2(as.formula(paste0(names(dataset)[length(names(dataset))],'~.')),data=train,test = test,seed = k)
    result_mat_bart[i,test_index]<-result$yhat.test.mean
    result_mat_bart_train[i,index] <- result$yhat.train.mean #QUESTION ED: why use index here and not trainindex
    result_mat_bart[i,ncol(result_mat_evtree)]<-k
    result_mat_bart_train[i,ncol(result_mat_evtree)]<-k
    #born again
    start<-Sys.time()
    s_train<-data.frame(matrix(nrow = 10*nrow(train),ncol=ncol(train)))
    names(s_train)<-names(train)
    for (j in 1:(10*nrow(train))) {
      s_train[j,]<-smearing(train)
    }

    s_train[,ncol(s_train)]<-bart2(as.formula(paste0(names(dataset)[length(names(dataset))],'~.')),data=train,test = s_train,seed = k)$yhat.test.mean 
    
    model_rpart_born<-rpart(as.formula(paste0(names(dataset)[length(names(dataset))],'~.')), data=s_train,maxdepth=maxd, control = control2) 
    #Remark ED: you did not specify the control here in the rpart analysis: why not use the same control here as above? I added this to the code. Do you agree with this change?
    node_born[i]<-sum(model_rpart_born[["frame"]][["var"]]=='<leaf>')
    end<-Sys.time()
    time_bo[i]<-start-end
    #model_rpart_born=prune(model_rpart_born,cp= model_rpart_born$cptable[which.min(model_rpart_born$cptable[,"xerror"]),"CP"])
    result_mat_born[i,test_index] <-predict(model_rpart_born, newdata= test)
    result_mat_born_train[i,train_index] <-predict(model_rpart_born, newdata= dataset[train_index,])
    result_mat_born[i,ncol(result_mat_evtree)]<-k
    result_mat_born_train[i,ncol(result_mat_evtree)]<-k
    i<-i+1
    }
  #Compute mse (not yet tested whether this works)
  #obj<-list(result_mat_rpart=result_mat_rpart,result_mat_rpart_train=result_mat_rpart_train,result_mat_bart=result_mat_bart,result_mat_bart_train=result_mat_bart_train,result_mat_born=result_mat_born,result_mat_born_train=result_mat_born_train,result_mat_evtree=result_mat_evtree,result_mat_evtree_train=result_mat_evtree_train,node_rpart=node_rpart,node_born=node_born,node_evtree=node_evtree)
  #for prediction
  #obj<-list(mse_evtree=mse_evtree,mse_rpart=mse_rpart,mse_bart=mse_bart,node_evtree=node_evtree,node_rpart=node_rpart,node_born=node_born)
  #for time
  obj<-list(time_bo=time_bo,time_ev=time_ev,time_rp=time_rp)
  return(obj)
}