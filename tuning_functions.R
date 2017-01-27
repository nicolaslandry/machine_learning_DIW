## TUNING.DATASET
## NICOLAS LANDRY  created the 25/01/2017
## LAST MODIFICATION: 15/01/2017
##OBJECTIVE: Only one code for all data set creation

#=====================================================
#=============== 1 FUNCTIONS DECLARATION =============
#=====================================================

ids.bootstrap.up<-function(ids,ratioGT,y){
  idsG<-ids[which(y=="GCR")]
  idsL<-ids[which(y=="LWR")]
  nLWR<-length(idsL)
  if(length(idsG)>1){
    idsG2<-sample(x = idsG,size = (ratioGT/(1-ratioGT))*nLWR,replace = TRUE)
  }else{
    idsG2=rep(idsG,(ratioGT/(1-ratioGT))*nLWR)
  }
  ids2<-c(idsG2,idsL)
  #print(length(idsG2)/length(ids2))
  ids2
}

ids.bootstrap.do<-function(y,ratioGT,size){
  ids <-1:length(y)
  idsG<-ids[which(y=="GCR")]
  idsL<-ids[which(y=="LWR")]
  
  idsG2<-sample(x = idsG,size = ceiling(ratioGT*size),replace = FALSE)
  #idsL2<-sample(x = idsL,size = length(idsG),replace = FALSE) # 50/50 sampling
  idsL2<-sample(x = idsL,size = size-ceiling(ratioGT*size),replace = FALSE)# ratioGT/(1-ratioGT) sampling
  ids2<-c(idsG2,idsL2)
  ids2
}

activate_parallel_mode<-function(cores_limit=25){
  max_cores <- detectCores()[1]
  if (max_cores > cores_limit) max_cores <- cores_limit
  cl <- makeCluster(max_cores, homogeneous=T)
  registerDoParallel(cl)
}

error_matrices<-function(nrow,ncol,seq_trees=1:nrow){
  M<-matrix(NA, nrow,ncol);
  rownames(M)=sprintf("%strees",seq_trees)
  return(list(tot_error=M,gcr_error=M,lwr_error=M))
}

train_test_sets<-function(d,rep){
  error=1
  while(error==1){
    ids.train=sample(1:nrow(d),size = rep$.tsetsize,replace = F)
    if(length(which(d$type[ids.train]=="GCR"))>0) error =0
  }
  if(rep$.up){
    ids.train.up=ids.bootstrap.up(ids.train,y=d$type[ids.train],ratioGT = rep$.ratio)
    test.set=d[-ids.train.up,]
    train.set=d[ids.train.up,]
  }else{
    ids.train.do=ids.bootstrap.do(d$type,ratioGT = rep$.ratio, size=rep$.tsetsize)
    test.set=d[-ids.train.do,]
    train.set=d[ids.train.do,]
  }
  return(list(test=test.set,train=train.set))
}


error_TOT<-function(pred,test,opt_t=NA){
  if(is.na(opt_t)){
    return(length(which((pred=="LWR" & test=="GCR")|(pred=="GCR" & test=="LWR")))/length(pred)) 
  }else{
    return(length(which((pred>opt_t & test=="GCR")|(pred<opt_t & test=="LWR")))/length(pred)) 
  }
}

error_GCR<-function(pred,test,opt_t=NA){
  if(is.na(opt_t)){
    return(length(which(pred=="LWR" & test=="GCR"))/length(which(test=="GCR")))
  }else{
    return(length(which(pred>opt_t & test=="GCR"))/length(which(test=="GCR"))) 
  }
}

error_LWR<-function(pred,test,opt_t=NA){
  if(is.na(opt_t)){
    return(length(which(pred=="GCR" & test=="LWR"))/length(which(test=="LWR")))
  }else{
    return(length(which(pred<opt_t & test=="LWR"))/length(which(test=="LWR"))) 
  }
}

error_stat<-function(M){
  return(rbind(apply(M,1,mean),apply(M,1,sd)))
}

opt_t<-function(pred,test){
  analysis <- roc(response=test$type, predictor=pred)
  e <- cbind(analysis$thresholds,analysis$sensitivities*analysis$specificities)
  opt_t <- subset(e,e[,2]==max(e[,2]))[1,1]
  return(opt_t)
}


#=====================================================
#=============== 2 LOAD OF THE DATASET ===============
#=====================================================

cat("load of the dataset \n")
alldata=read.csv(file="Pris_data_all_with_outages_v12_nl.csv",sep=";")
ids=which(alldata$type=="PWR"|alldata$type=="BWR")
levels(alldata$type)=c(levels(alldata$type),"LWR")
alldata$type[ids]="LWR"
ids.japan=which(alldata$country=="JAPAN" & alldata$year>2010)
alldata=alldata[-ids.japan,]

colnames=c("type","year","planned_outages","t11","t12","t13","t14","t15","t16","t17","t21","t31","t32","t33","t35","t41","t42","t43")
cnames=which(is.element(colnames(alldata),colnames))
countries=c("FRANCE","GERMANY","JAPAN","SPAIN","SWITZERLAND","UNITED KINGDOM","UNITED STATES OF AMERICA")
ids=which((alldata$type=="LWR"|alldata$type=="GCR") & alldata$country %in% countries)
d=alldata[ids,c(cnames)]
d$type=factor(d$type)
d=na.omit(d)

#Removal of zero year outages
S<-rep(0,nrow(d))
for(i in 1:nrow(d)){
  S[i]=(sum(d[i,4:(ncol(d)-1)]))
}
d<-d[-which(S==0),]

#normalisation
for(r in 1:nrow(d)){
  s=sum(d[r,4:ncol(d)])
  if(d$year[r] %% 4 ==0){
    for(c in 4:ncol(d)){
      d[r,c]=d[r,c]/(8784-s+d[r,c])
      if(d[r,c]=="NaN"){d[r,c] <-      0} 
    }
  }else{
    for(c in 4:ncol(d)){
      d[r,c]=d[r,c]/(8760-s+d[r,c])
      if(d[r,c]=="NaN"){d[r,c] <-      0} 
    }
  }
}

d<-d[,-c(1,3)]
rm(colnames,alldata,s,S,i,ids,cnames,ids.japan,countries,c,r)
