smearing<-function(data)
{ 
  m<-sample(1:nrow(data),1)
  Xm<-data[m,]
  index<-sample(1:nrow(data),(ncol(data)-1),replace = T)
  for (j in 1:(ncol(data)-1)) {
    index[j]<-sample(c(index[j],m),1)
    Xm[j]<-data[index[j],j]
  }
  return(Xm)
}