## TUNING RANDOM FOREST
## NICOLAS LANDRY  created the 23/11/2016
## LAST MODIFICATION: 12/01/2017
##OBJECTIVE: How to improve result from rf?


rm(list=ls())

PARALLEL=FALSE
THRESHOLD=FALSE
#setwd("H:\\Machine_Learning\\")
#setwd("/projekte/sseifert/homes/Machine_Learning/RANDOM FOREST")

setwd("H:\\Machine_Learning\\RANDOM FOREST\\")
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(randomForest)
library(mlbench)
library(caret)
library(doParallel)

#=====================================================
#================= 2 INITIALIZATION ==================
#=====================================================
source("C:/Users/nlandry/Documents/Nicolas/05-Outages - with Stephan/R-codes/tuning_dataset.R")
#=====================================================
#================== 3 ANALYSIS =======================
#=====================================================
#PARALLEL PROCESSING
if(PARALLEL) activate_parallel_mode(cores_limit = 7)


seed=7
set.seed(seed)

tunegrid.up <- expand.grid(.ntree=2800,
                           .mtry=seq(1,5,1),
                           .classwt=seq(8,248,16),
                           .maxnodes=seq(2,14,2),
                           .sampsize=seq(50,1100,75),
                           .ratio=c(1),
                           .up=c(TRUE))
tunegrid.do <- expand.grid(.ntree=2800,
                           .mtry=seq(1,5,1),
                           .classwt=1,
                           .maxnodes=seq(2,14,2),
                           .sampsize=seq(50,200,25),
                           .ratio=seq(0.10,0.8,0.10),
                           .up=c(FALSE))
tunegrid.do <- tunegrid.do[(tunegrid.do$.ratio*tunegrid.do$.sampsize)<sum(d$type=="GCR"),]
tunegrid <- rbind(tunegrid.up,tunegrid.do)

k <- nrow(tunegrid)



#### WITH FOREACH LOOP
foreach(count=1:k, .packages="randomForest")%dopar%{
  #for(count in 8401:k){
  param <- tunegrid[count,]
  
  tot_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
  gcr_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
  lwr_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
  
  if(param$.up){
    
    
    for (i in 1:30){
      cat(paste(count,",",i,"/n",sep = ""))
      rf <- randomForest(type~.,
                         data=d,
                         ntree=param$.ntree,
                         mtry=param$.mtry,
                         classwt=c(param$.classwt+1000,1000-param$.classwt),
                         sampsize=param$.sampsize,
                         maxnodes=param$.maxnodes)$err.rate
      tot_error[,i] <- rf[,1]
      gcr_error[,i] <- rf[,2]
      lwr_error[,i] <- rf[,3]
    }
    
    tot_stat <- rbind(apply(as.matrix(tot_error[seq(200,rep$.ntree,100),]),1,mean),
                      apply(as.matrix(tot_error[seq(200,rep$.ntree,100),]),1,sd))
    gcr_stat <- rbind(apply(gcr_error[seq(200,rep$.ntree,100),],1,mean),
                      apply(gcr_error[seq(200,rep$.ntree,100),],1,sd))
    lwr_stat <- rbind(apply(lwr_error[seq(200,rep$.ntree,100),],1,mean),
                      apply(lwr_error[seq(200,rep$.ntree,100),],1,sd))
  }else{
    
    
    for (i in 1:30){
      cat(paste(count,",",i,"/n",sep = ""))
      
      print("GCRs in the training set")
      
      rf <- randomForest(type~.,data=d,
                         ntree=param$.ntree,
                         mtry=param$.mtry,
                         classwt=c(1000,1000),
                         #strata=as.factor(c("GCR","LWR")),
                         strata=d$type,
                         sampsize=c(ceiling(param$.ratio*param$.sampsize), param$.sampsize-ceiling(param$.ratio*param$.sampsize)),
                         maxnodes=param$.maxnodes
                         ,keep.inbag=T
      )$err.rate
      tot_error[,i] <- rf[,1]
      gcr_error[,i] <- rf[,2]
      lwr_error[,i] <- rf[,3]
    }
    tot_stat <- rbind(apply(as.matrix(tot_error[seq(200,rep$.ntree,100),]),1,mean),
                      apply(as.matrix(tot_error[seq(200,rep$.ntree,100),]),1,sd))
    gcr_stat <- rbind(apply(gcr_error[seq(200,rep$.ntree,100),],1,mean),
                      apply(gcr_error[seq(200,rep$.ntree,100),],1,sd))
    lwr_stat <- rbind(apply(lwr_error[seq(200,rep$.ntree,100),],1,mean),
                      apply(lwr_error[seq(200,rep$.ntree,100),],1,sd))
  }
  
  result <- list(rbind(tot_stat,gcr_stat,lwr_stat),param)
  
  #======================
  ## TO CHANGE
  save(result,file = paste("2- RESULTS/RF_output_",count,".RData",sep=""))
  #======================
}
