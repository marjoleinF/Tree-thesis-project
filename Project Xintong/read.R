start=1001

for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
  for (j in c(3,4,5)) {
    list_name<-paste(i,j ,sep='')
    assign(list_name,list(rpart=numeric(0),rpart_train=numeric(0),bart=numeric(0),bart_train=numeric(0),born=numeric(0),born_train=numeric(0),evtree=numeric(0),evtree_train=numeric(0),node_rpart=numeric(0),node_born=numeric(0),node_evtree=numeric(0)))
    now<-get(list_name)
    for (start in seq(1001,1241,by=10)) {
    path=paste('result',i,j, "Start", start, ".RData",sep='')
    load(path)
    ls()
    
    for (m in 1:8) {
      now[[m]]<-rbind(now[[m]],get(paste0('now_result',j))[[m]])

    }
    for (m in 9:11) {
      now[[m]]<-cbind(now[[m]],get(paste0('now_result',j))[[m]])
    }

    }
    assign(list_name,now)
  }
  
}
rm(now)
rm(now_result3)
rm(now_result4)
rm(now_result5)