## TUNING CLASSIFICATION TREE
## NICOLAS LANDRY  created the 15/12/2016
## LAST MODIFICATION: 15/12/2016
##OBJECTIVE: How to improve result from ct?


rm(list=ls())
setwd("H:\\CLASSIFICATION\\")
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(rpart)

#=====================================================
#=============== 3 FUNCTIONS DECLARATION =============
#=====================================================
ids.bootstrap.up<-function(ids,ratioGT,y){
  idsG<-ids[which(y=="GCR")]
  idsL<-ids[which(y=="LWR")]
  nLWR<-length(idsL)
  idsG2<-sample(x = idsG,size = (ratioGT/(1-ratioGT))*nLWR,replace = TRUE)
  ids2<-c(idsG2,idsL)
  #print(length(idsG2)/length(ids2))
  ids2
}

ids.bootstrap.do<-function(ratioGT,y,size){
  ids <-1:length(y)
  idsG<-ids[which(y=="GCR")]
  idsL<-ids[which(y=="LWR")]
  
  idsG2<-sample(x = idsG,size = size/2,replace = FALSE)
  idsL2<-sample(x = idsL,size = size/2,replace = FALSE)
  #print(idsG2)
  ids2<-c(idsG2,idsL2)
  #print(length(idsG2)/length(ids2))
  ids2
}

#=====================================================
#=============== 4 LOAD OF THE DATASET ===============
#=====================================================
cat("load of the dataset \n")
alldata=read.csv(file="prisv11.csv",sep=";")
ids=which(alldata$type=="PWR"|alldata$type=="BWR")
levels(alldata$type)=c(levels(alldata$type),"LWR")
alldata$type[ids]="LWR"

colnames=c("type","t11","t12","t13","t14","t15","t16","t17","t21","t31","t32","t33","t35","t41","t42","t43")
#colnames=c("type","t12","t13","t14","t15","t16","t17","t21","t31","t32","t33","t35","t41","t42","t43")
cnames=which(is.element(colnames(alldata),colnames))
ids=which(alldata$type=="LWR"|alldata$type=="GCR")

d=alldata[ids,c(cnames)]
d$type=factor(d$type)
d=na.omit(d)

#Removal of zero year outages
S<-rep(0,nrow(d))
for(i in 1:nrow(d)){
  S[i]=(sum(d[i,2:(ncol(d)-1)]))
}
d<-d[-which(S==0),]
rm(S)
rm(i)
rm(ids)
rm(cnames)
#=====================================================
#================== 5 ANALYSIS =======================
#=====================================================


seed=7
set.seed(seed)

#### WITH FOREACH LOOP

nGCR=length(which(d$type=="GCR"))
tunegrid.up <- expand.grid(.minsplit=seq(3,40,2),.minbucket=seq(1,20,2),.cp=c(0.00001,0.0001,0.001,0.01,0.1),.tsetsize=seq(100,1100,80),.ratio=seq(0.1,0.8,0.1),.up=TRUE)
tunegrid.do <- expand.grid(.minsplit=seq(3,40,2),.minbucket=seq(1,20,2),.cp=c(0.00001,0.0001,0.001,0.01,0.1),.tsetsize=seq(10,200,20),.ratio=0.5,.up=FALSE)
tunegrid=rbind(tunegrid.up,tunegrid.do)

tunegridbest=data.frame(.minsplit=9,.minbucket=3,.cp=0.01,.tsetsize=190,.ratio=0.5,.up=FALSE)

rep     <- tunegridbest


control <- rpart.control(minsplit = rep$.minsplit,
                         minbucket = rep$.minbucket,
                         cp = rep$.cp)

oob_error <- matrix(NA, nrow=300,ncol=1)
gcr_error <- matrix(NA, nrow=300,ncol=1)
lwr_error <- matrix(NA, nrow=300,ncol=1)


outF=matrix(NA,1,15)
colnames(outF)=allVars

for (i in 1:1000){
  print(i)
  if(rep$.up){
    ids.train=sample(1:nrow(d),size = rep$.tsetsize,replace = F)
    ids.train.up=ids.bootstrap.up(ids.train,d$type[ids.train],ratioGT = rep$.ratio)
    test.set=d[-ids.train.up,]
    train.set=d[ids.train.up,]
  }else{
    ids.train.do=ids.bootstrap.do(d$type,ratioGT = rep$.ratio, size=rep$.tsetsize)
    test.set=d[-ids.train.do,]
    train.set=d[ids.train.do,]
  }
  
  ct <- rpart(type~., data=train.set, control = control)
  
  fit=ct
  tmp=rownames(fit$splits)
  allVars=colnames(attributes(fit$terms)$factors)  
  rownames(fit$splits)=1:nrow(fit$splits)
  splits=data.frame(fit$splits)
  splits$var=tmp
  splits$type=""
  frame=as.data.frame(fit$frame)
  index=0
  for(i in 1:nrow(frame)){
    if(frame$var[i] != "<leaf>"){
      index=index + 1
      splits$type[index]="primary"
      if(frame$ncompete[i] > 0){
        for(j in 1:frame$ncompete[i]){
          index=index + 1
          splits$type[index]="competing"}}
      if(frame$nsurrogate[i] > 0){
        for(j in 1:frame$nsurrogate[i]){
          index=index + 1
          splits$type[index]="surrogate"}}}}
  splits$var=factor(as.character(splits$var))
  splits=subset(splits, type != "surrogate")
  out=aggregate(splits$improve,
                list(Variable = splits$var),
                sum, na.rm = TRUE)
  allVars=colnames(attributes(fit$terms)$factors)
  if(!all(allVars %in% out$Variable)){
    missingVars=allVars[!(allVars %in% out$Variable)]
    zeros=data.frame(x = rep(0, length(missingVars)), Variable = missingVars)
    out=rbind(out, zeros)}
  out2=data.frame(Overall = out$x)
  rownames(out2)=out$Variable
  outF=rbind(outF,t(out2))
  
}
outF=outF[-1,]
outFF=rbind(apply(outF,2,mean)-apply(outF,2,sd),2*apply(outF,2,sd))
barplot(outFF)
barplot(apply(outF,2,mean))
plot(density(outF[,5]))

gcr_error2=c(gcr_error,gcr_error2)
gcr_error=gcr_error2
rbind(oob_list,gcr_list,lwr_list)
plot(gcr_error)
plot(cumsum(gcr_error)/seq_along(gcr_error))
library(TTR)
plot(runSD(gcr_error,cumulative = TRUE))
#SAVING
#write.table(rf_gridsearch$results,"result_tuning_rf.txt")




