library(DescTools)
library(stringr)
library(ggplot2)
library(grid)
#compare bart
box_3<-dep_3[dep_3$X3!='bart',]
d3<-ggplot(box_3, aes(x=X1, y=X2)) + 
  xlab('relative difference of the mse')+
  ylab('')+
  geom_boxplot()
n3<-ggplot(box_3, aes(x=X4, y=X2)) + 
  xlab('complexity')+
  ylab('')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_boxplot()
grid.arrange(arrangeGrob(d3,n3, ncol=2, widths=c(4,1.5)))

#compare rpart

dep_3_rpart<-dep_3[dep_3['X3']!='rpart',]
d3<-ggplot(dep_3_rpart, aes(x=X5, y=X2)) + 
  xlab('relative difference of the mse')+
  ylab('')+
  geom_boxplot()
n3<-ggplot(dep_3_rpart, aes(x=X7, y=X2)) + 
  xlab('complexity')+
  ylab('')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_boxplot()
grid.arrange(arrangeGrob(d3,n3, ncol=2, widths=c(4,1.5)))

dep_4_rpart<-dep_4[dep_4['X3']!='rpart',]
d4<-ggplot(dep_4_rpart, aes(x=X5, y=X2)) + 
  xlab('relative difference of the mse')+
  ylab('')+
  geom_boxplot()
n4<-ggplot(dep_4_rpart, aes(x=X7, y=X2)) + 
  xlab('complexity')+
  ylab('')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_boxplot()
grid.arrange(arrangeGrob(d4,n4, ncol=2, widths=c(4,1.5)))

dep_5_rpart<-dep_5[dep_5['X3']!='rpart',]
d5<-ggplot(dep_5_rpart, aes(x=X5, y=X2)) + 
  xlab('relative difference of the mse')+
  ylab('')+
  geom_boxplot()
n5<-ggplot(dep_5_rpart, aes(x=X7, y=X2)) + 
  xlab('complexity')+
  ylab('')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_boxplot()
grid.arrange(arrangeGrob(d5,n5, ncol=2, widths=c(4,1.5)))

#compare evtree
dep_3_evtree<-dep_3[dep_3['X3']=='born',]
d3<-ggplot(dep_3_evtree, aes(x=X6, y=X2)) + 
  xlab('relative difference of the mse')+
  ylab('')+
  geom_boxplot()
n3<-ggplot(dep_3_evtree, aes(x=X8, y=X2)) + 
  xlab('complexity')+
  ylab('')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_boxplot()
grid.arrange(arrangeGrob(d3,n3, ncol=2, widths=c(4,1.5)))

dep_4_evtree<-dep_4[dep_4['X3']=='born',]
d4<-ggplot(dep_4_evtree, aes(x=X6, y=X2)) + 
  xlab('relative difference of the mse')+
  ylab('')+
  geom_boxplot()
n4<-ggplot(dep_4_evtree, aes(x=X8, y=X2)) + 
  xlab('complexity')+
  ylab('')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_boxplot()
grid.arrange(arrangeGrob(d4,n4, ncol=2, widths=c(4,1.5)))

dep_5_evtree<-dep_5[dep_5['X3']=='born',]
d5<-ggplot(dep_3_evtree, aes(x=X6, y=X2)) + 
  xlab('relative difference of the mse')+
  ylab('')+
  geom_boxplot()
n5<-ggplot(dep_3_evtree, aes(x=X8, y=X2)) + 
  xlab('complexity')+
  ylab('')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_boxplot()
grid.arrange(arrangeGrob(d5,n5, ncol=2, widths=c(4,1.5)))
#compare bart
for (j in 3:5) {
  table_name=paste('test_table',j,sep = '_')
  test_table<-data.frame(matrix(nrow=18,ncol = 6))
  now_data<-get(paste('dep',j,sep = '_'))
  names(test_table)<-c('data','diff','lwr.ci','upr.ci','mean','method')
  n=1
  for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
    a<-DunnettTest(x=now_data[now_data$X9==i,]$X1, g=as.factor(now_data[now_data$X9==i,]$X2))
    test_table[n:(n+2),2:5]<-a[[1]]
    row_name<-row.names(a[[1]])
    row_name[1]<-str_split(row.names(a[[1]])[1],pattern = '-')[[1]][1]
    row_name[2]<-str_split(row.names(a[[1]])[2],pattern = '-')[[1]][1]
    row_name[3]<-str_split(row.names(a[[1]])[3],pattern = '-')[[1]][1]
    test_table[n:(n+2),1]<-row_name
    n=n+3
  }
  test_table[,6]<-unlist(str_split(test_table[,1],pattern = '_'))[seq(2,18,2)]
  assign(table_name,test_table)
}
test_table[11,]<-test_table_3[11,]
ggplot(data=test_table,aes(x=data, y=diff,colour=method)) +
  geom_hline(yintercept=0, linetype="11", colour="grey60") +
  geom_segment(aes(xend=data, y=lwr.ci, yend=upr.ci), size=0.4, 
               arrow=arrow(ends="both", length=unit(0.05, "inches"), angle=70)) + 
  geom_point() +
  coord_flip() +
  xlab('')+
  ylab('relative change in mean squared error (%)')+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme_classic()+ scale_y_continuous(labels = scales::percent)

#compare rpart
#mse
for (j in 3:5) {
  table_name=paste('test_table',j,sep = '_')
  test_table<-data.frame(matrix(nrow=12,ncol = 6))
  now_data<-get(paste('dep',j,sep = '_'))
  now_data<-now_data[now_data$X3!='bart',]
  names(test_table)<-c('data','diff','lwr.ci','upr.ci','mean','method')
  n=1
  for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
    a<-DunnettTest(x=now_data[now_data$X9==i,]$X5, g=as.factor(now_data[now_data$X9==i,]$X2),control = paste(i,'rpart',sep = '_'))
    test_table[n:(n+1),2:5]<-a[[1]]
    row_name<-row.names(a[[1]])
    row_name[1]<-str_split(row.names(a[[1]])[1],pattern = '-')[[1]][1]
    row_name[2]<-str_split(row.names(a[[1]])[2],pattern = '-')[[1]][1]
    
    test_table[n:(n+1),1]<-row_name
    n=n+2
  }
  test_table[,6]<-unlist(str_split(test_table[,1],pattern = '_'))[seq(2,24,2)]
  assign(table_name,test_table)
}
#node
for (j in 3:5) {
  table_name=paste('test_table_node',j,sep = '_')
  test_table<-data.frame(matrix(nrow=12,ncol = 6))
  now_data<-get(paste('dep',j,sep = '_'))
  now_data<-now_data[now_data$X3!='bart',]
  names(test_table)<-c('data','diff','lwr.ci','upr.ci','mean','method')
  n=1
  for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
    a<-DunnettTest(x=now_data[now_data$X9==i,]$X7, g=as.factor(now_data[now_data$X9==i,]$X2),control = paste(i,'rpart',sep = '_'))
    test_table[n:(n+1),2:5]<-a[[1]]
    row_name<-row.names(a[[1]])
    row_name[1]<-str_split(row.names(a[[1]])[1],pattern = '-')[[1]][1]
    row_name[2]<-str_split(row.names(a[[1]])[2],pattern = '-')[[1]][1]
    
    test_table[n:(n+1),1]<-row_name
    n=n+2
  }
  test_table[,6]<-unlist(str_split(test_table[,1],pattern = '_'))[seq(2,24,2)]
  assign(table_name,test_table)
}

plot_compare_rpart<-function(dataset,dataset_node){
a<-ggplot(data=dataset,aes(x=data, y=diff,colour=method)) +
  geom_hline(yintercept=0, linetype="11", colour="grey60") +
  geom_segment(aes(xend=data, y=lwr.ci, yend=upr.ci), size=0.4, 
               arrow=arrow(ends="both", length=unit(0.05, "inches"), angle=70)) + 
  geom_point() +
  coord_flip() +
  xlab('')+
  ylab('relative change in mean squared error (%)')+
  #theme(axis.title.y=element_blank(),
  #axis.text.y=element_blank(),
  #axis.ticks.y=element_blank())+
  #scale_fill_discrete_qualitative(palette = "cold")+
  
  theme_classic()+
  theme(legend.position = c(0.2,0.9))+ scale_y_continuous(labels = scales::percent)

b<-ggplot(data=dataset_node,aes(x=data, y=diff,colour=method)) +
  geom_hline(yintercept=0, linetype="11", colour="grey60") +
  geom_segment(aes(xend=data, y=lwr.ci, yend=upr.ci), size=0.4, 
               arrow=arrow(ends="both", length=unit(0.05, "inches"), angle=70)) + 
  geom_point() +
  coord_flip() +
  xlab('')+
  ylab('relative change in complexity (%)')+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),legend.position = "none")+ scale_y_continuous(labels = scales::percent)
grid.arrange(arrangeGrob(a,b, ncol=2, widths=c(4,1.5)))}
#compare evtree
#mse
for (j in 3:5) {
  table_name=paste('test_table',j,sep = '_')
  test_table<-data.frame(matrix(nrow=6,ncol = 5))
  now_data<-get(paste('dep',j,sep = '_'))
  now_data<-now_data[(now_data$X3=='born' | now_data$X3=='evtree'| now_data$X3=='rpart'),]
  names(test_table)<-c('data','diff','lwr.ci','upr.ci')
  n=1
  for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
    a<-DunnettTest(x=now_data[now_data$X9==i,]$X6, g=as.factor(now_data[now_data$X9==i,]$X2),control = paste(i,'evtree',sep = '_'))
    test_table[n,2:5]<-a[[1]]
    row_name<-row.names(a[[1]])
    row_name[1]<-str_split(row.names(a[[1]])[1],pattern = '-')[[1]][1]
    #row_name[2]<-str_split(row.names(a[[1]])[2],pattern = '-')[[1]][1]
    
    test_table[n,1]<-row_name
    n=n+1
  }
  assign(table_name,test_table)
}
#node

for (j in 3:5) {
  table_name=paste('test_table_node',j,sep = '_')
  test_table<-data.frame(matrix(nrow=6,ncol = 5))
  now_data<-get(paste('dep',j,sep = '_'))
  now_data<-now_data[(now_data$X3=='born' | now_data$X3=='evtree'),]
  names(test_table)<-c('data','diff','lwr.ci','upr.ci')
  n=1
  for (i in c('BostonHousing','Servo','Hitters','cpus','Auto','Concrete')) {
    a<-DunnettTest(x=now_data[now_data$X9==i,]$X8, g=as.factor(now_data[now_data$X9==i,]$X2),control = paste(i,'evtree',sep = '_'))
    test_table[n,2:5]<-a[[1]]
    row_name<-row.names(a[[1]])
    row_name[1]<-str_split(row.names(a[[1]])[1],pattern = '-')[[1]][1]
    #row_name[2]<-str_split(row.names(a[[1]])[2],pattern = '-')[[1]][1]
    
    test_table[n,1]<-row_name
    n=n+1
  }
  assign(table_name,test_table)
}

plot_compare_evtree<-function(dataset,dataset_node){
a<-ggplot(data=dataset,aes(x=data, y=diff)) +
  geom_hline(yintercept=0, linetype="11", colour="grey60") +
  geom_segment(aes(xend=data, y=lwr.ci, yend=upr.ci), size=0.4, 
               arrow=arrow(ends="both", length=unit(0.05, "inches"), angle=70)) + 
  geom_point() +
  coord_flip() +
  xlab('')+
  ylab('relative change in mean squared error (%)')+
  #theme(axis.title.y=element_blank(),
  #axis.text.y=element_blank(),
  #axis.ticks.y=element_blank())+
  theme_classic()+ scale_y_continuous(labels = scales::percent)

b<-ggplot(data=dataset_node,aes(x=data, y=diff)) +
  geom_hline(yintercept=0, linetype="11", colour="grey60") +
  geom_segment(aes(xend=data, y=lwr.ci, yend=upr.ci), size=0.4, 
               arrow=arrow(ends="both", length=unit(0.05, "inches"), angle=70)) + 
  geom_point() +
  coord_flip() +
  xlab('')+
  ylab('relative change in complexity (%)')+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+ scale_y_continuous(labels = scales::percent)
grid.arrange(arrangeGrob(a,b, ncol=2, widths=c(4,1.5)))}

# plot computation time
tree_depth=c(3,4,5)
#times_Concrete_5[[2]][times_Concrete_5[[2]]>-3]<-times_Concrete_5[[2]][times_Concrete_5[[2]]>-3]*60
computation_time=c(-mean(times_Concrete_3[[2]]),-mean(times_Concrete_4[[2]]),-mean(times_Concrete_5[[2]]))
plot(tree_depth,computation_time,type='b',col='blue',ylab='Computation Time (s)')
computation_time=c(-mean(times_Concrete_3[[1]]),-mean(times_Concrete_4[[1]]),-mean(times_Concrete_5[[1]]))
lines(tree_depth,computation_time,type='b',col='red')
legend("topleft", legend=c("born","evtree"),        #Í¼ÀıÄÚÈİ
       col=c("red","blue"),                 #Í¼ÀıÑÕÉ«
       lty=1,lwd=2)