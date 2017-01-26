## TUNING RANDOM FOREST
## NICOLAS LANDRY  created the 23/11/2016
## LAST MODIFICATION: 12/01/2017
##OBJECTIVE: How to improve result from rf?


rm(list=ls())
#setwd("H:\\Machine_Learning\\")
#setwd("/projekte/sseifert/homes/Machine_Learning")

setwd("H:\\RANDOM FOREST\\")
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(randomForest)
library(mlbench)
library(caret)
library(doParallel)

#=====================================================
#=============== 3 FUNCTIONS DECLARATION =============
#=====================================================
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree","classwt","maxnodes","sampsize"), class = rep("numeric", 5), label = c("mtry", "ntree","classwt","maxnodes","sampsize"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  compteur<<-compteur+1
  print(paste(compteur,"/",iter," ",param$ntree," ",param$mtry," ",param$classwt," ",param$maxnodes," ",param$sampsize))
  classwt=c(param$classwt+1000,1000-param$classwt)
  randomForest(x, y,
               mtry = param$mtry,
               sampsize =param$sampsize,
               ntree=param$ntree,
               maxnodes=param$maxnodes,
               classwt = classwt,replace = TRUE)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


ids.bootstrap.do<-function(ratioGT,y,size){
  ids <-1:length(y)
  idsG<-ids[which(y=="GCR")]
  idsL<-ids[which(y=="LWR")]
  
  idsG2<-sample(x = idsG,size = length(idsG),replace = FALSE)
  idsL2<-sample(x = idsL,size = length(idsG),replace = FALSE) # 50/50 sampling
  #idsL2<-sample(x = idsL,size = (1-ratioGT)/ratioGT*length(idsG),replace = FALSE)# ratioGT/(1-ratioGT) sampling
  ids2<-c(idsG2,idsL2)
  ids2
}

#=====================================================
#=============== 4 LOAD OF THE DATASET ===============
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
rm(s,S,i,ids,cnames,ids.japan,countries,c,r)

#=====================================================
#================== 5 ANALYSIS =======================
#=====================================================

#PARALLEL PROCESSING
max_cores <- detectCores()[1]
max_cores
if (max_cores > 25) max_cores <- 25
cl <- makeCluster(max_cores, homogeneous=T)
registerDoParallel(cl)


seed=7
set.seed(seed)

tunegrid <- expand.grid(.ntree=1800,
                        .mtry=seq(1,5,1),
                        .classwt=seq(1,248,4),
                        .maxnodes=seq(2,18,2),
                        .sampsize=seq(50,1100,80),
                        .up=c(TRUE,FALSE))
k <- nrow(tunegrid)


#### WITH FOREACH LOOP
#foreach(count=1:k, .packages="randomForest")%dopar%{
for(count in 1:k){
  result<-list()
  rep <- tunegrid[count,]
  
  if(rep$.up){
    oob_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
    gcr_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
    lwr_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
    
    for (i in 1:30){
      cat(paste(count,",",i,"/n",sep = ""))
      rf <- randomForest(type~.,
                         data=d,
                         ntree=rep$.ntree,
                         mtry=rep$.mtry,
                         classwt=c(rep$.classwt+1000,1000-rep$.classwt),
                         sampsize=rep$.sampsize,
                         maxnodes=rep$.maxnodes)$err.rate
      oob_error[,i] <- rf[,1]
      gcr_error[,i] <- rf[,2]
      lwr_error[,i] <- rf[,3]
    }
    
    oob_list <- rbind(apply(as.matrix(oob_error[seq(200,rep$.ntree,100),]),1,mean),
                      apply(as.matrix(oob_error[seq(200,rep$.ntree,100),]),1,sd))
    gcr_list <- rbind(apply(gcr_error[seq(200,rep$.ntree,100),],1,mean),
                      apply(gcr_error[seq(200,rep$.ntree,100),],1,sd))
    lwr_list <- rbind(apply(lwr_error[seq(200,rep$.ntree,100),],1,mean),
                      apply(lwr_error[seq(200,rep$.ntree,100),],1,sd))
  }else{
    oob_error <- matrix(NA, nrow=length(seq(200,rep$.ntree,100)),ncol=30)
    gcr_error <- matrix(NA, nrow=length(seq(200,rep$.ntree,100)),ncol=30)
    lwr_error <- matrix(NA, nrow=length(seq(200,rep$.ntree,100)),ncol=30)
    
    for (i in 1:30){
      cat(paste(count,",",i,"/n",sep = ""))
      error=1
      while(error==1){
        ids.train=sample(1:nrow(d),size = rep$.sampsize,replace = F)
        if(length(which(d$type[ids.train]=="GCR"))>0) error =0
      }
      print("GCRs in the training set")
      ids.train.do=ids.bootstrap.do(y = d$type,ratioGT = rep$.ratio, size=rep$.sampsize)
      test.set=d[-ids.train.do,]
      train.set=d[ids.train.do,]
      
      rf <- randomForest(type~.,data=,
                         ntree=rep$.ntree,
                         mtry=rep$.mtry,
                         classwt=c(rep$.classwt+1000,1000-rep$.classwt),
                         sampsize=rep$.sampsize,
                         maxnodes=rep$.maxnodes)
      
      for(s in 1:length(seq(200,rep$.ntree,100))){
        ntree=seq(200,rep$.ntree,100)[s]
        rf <- randomForest(type~.,data=d,
                           ntree=ntree,
                           mtry=rep$.mtry,
                           classwt=c(rep$.classwt+1000,1000-rep$.classwt),
                           sampsize=rep$.sampsize,
                           maxnodes=rep$.maxnodes)
        
        pred=predict(rf,test.set,type = "class")
        oob_error[s,i] <- length(which((pred=="LWR" & test.set$type=="GCR")|(pred=="GCR" & test.set$type=="LWR")))/length(pred)
        gcr_error[s,i] <- length(which(pred=="LWR" & test.set$type=="GCR"))/length(which(test.set$type=="GCR"))
        lwr_error[s,i] <- length(which(pred=="GCR" & test.set$type=="LWR"))/length(which(test.set$type=="LWR"))
      }
    }
    oob_list <- rbind(apply(as.matrix(oob_error),1,mean),
                      apply(as.matrix(oob_error),1,sd))
    gcr_list <- rbind(apply(gcr_error,1,mean),
                      apply(gcr_error,1,sd))
    lwr_list <- rbind(apply(lwr_error,1,mean),
                      apply(lwr_error,1,sd))
  }
  
  result[[1]] <- rbind(oob_list,gcr_list,lwr_list)
  result[[2]] <- rep
  
  #======================
  ## TO CHANGE
  save(result,file = paste("2- RESULTS/RF_output_",count,".RData",sep=""))
  #======================
}
