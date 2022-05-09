
smearing<-function(data)
  #data = dataset with outcome at the last column
{ 
  m<-sample(1:nrow(data),1)
  m
  Xm<-data[m,]
  index<-sample(1:nrow(data),(ncol(data)-1),replace = TRUE) #why take the number of columns here? =length of the sample
  for (j in 1:(ncol(data)-1)) { #for each column in the data set
    index[j]<-sample(c(index[j],m),1)
    ##either keep the original value of subject m on variable j, or randomly select one from variable j
    Xm[j]<-data[index[j],j]
  }
  return(Xm)
}


smearingED<-function(data)
  #data = dataset with outcome at the last column
{ 
 #create one row of my new data set
  n_new <-sample(1:nrow(data),1)
  x_n <- data[n_new,]
  vec <-runif(1:ncol(data)-1)
  vecnew <- vec > .50
  for (i in 1:length(vecnew)) { #for each column in the data set
    if(vecnew[i]==FALSE){
    x_n[i] <- sample(data[,i],1)
    }
  }
  return(x_n)
}
