## TUNING RANDOM FOREST
## NICOLAS LANDRY  created the 23/11/2016
## LAST MODIFICATION: 11/02/2017
##OBJECTIVE: How to improve result from rf?


rm(list=ls())

PARALLEL=FALSE
THRESHOLD=FALSE
#setwd("H:\\Machine_Learning\\")
#setwd("/projekte/sseifert/homes/Machine_Learning/RANDOM FOREST")

setwd("C:\\Users\\Nicolas Landry\\Documents\\RF DIW")
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(randomForest)
library(mlbench)
library(caret)
library(doParallel)
library(pROC)
#=====================================================
#================= 2 INITIALIZATION ==================
#=====================================================

source("C:\\Users\\Nicolas Landry\\Documents\\RF DIW\\tuning_functions.R")
#=====================================================
#================== 3 ANALYSIS =======================
#=====================================================
#PARALLEL PROCESSING
if(PARALLEL) activate_parallel_mode(cores_limit = 7)


seed=7
set.seed(seed)

tunegrid.up <- expand.grid(.ntree=2800,
                           .mtry=seq(1,5,1),
                           .classwt=seq(16,248,16),
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


TREES=seq(200,param$.ntree,100)

#### WITH FOREACH LOOP
foreach(count=1:k, .packages="randomForest")%dopar%{
  #for(count in 8401:k){
  param <- tunegrid[count,]
  
  tot_error <- matrix(NA, nrow=length(TREES),ncol=30)
  gcr_error <- matrix(NA, nrow=length(TREES),ncol=30)
  lwr_error <- matrix(NA, nrow=length(TREES),ncol=30)
  for (i in 1:30){
    print(paste(count,i))
    if(param$.up){
      rf <- randomForest(type~.,
                         data=d,
                         ntree=param$.ntree,
                         mtry=param$.mtry,
                         classwt=c(param$.classwt+1000,1000-param$.classwt),
                         sampsize=param$.sampsize,
                         maxnodes=param$.maxnodes,keep.inbag=TRUE)
    }else{
      rf <- randomForest(type~.,data=d,
                         ntree=param$.ntree,
                         mtry=param$.mtry,
                         classwt=c(1000,1000),
                         #strata=as.factor(c("GCR","LWR")),
                         strata=d$type,
                         sampsize=c(ceiling(param$.ratio*param$.sampsize), param$.sampsize-ceiling(param$.ratio*param$.sampsize)),
                         maxnodes=param$.maxnodes
                         ,keep.inbag=T)
    }
    IN=rf$inbag
    
    # optimal threshold to split
    Opt_t=NA
    if(THRESHOLD){
      
      pred=predict(rf,d,predict.all=TRUE)
      
      #oob error for different ntrees
      votes.oob=list()
      share.in<-trees.in<-rep(0,nrow(IN))
      
      for(t in 1:length(TREES)){
        
        ntree=TREES[t]
        if(t==1){
          IN2=IN[,1:ntree]
          PRED=pred$individual[,1:ntree]
        }else{
          IN2=cbind(IN2,IN[,(TREES[t-1]+1):ntree])
          PRED=cbind(PRED,pred$individual[,(TREES[t-1]+1):ntree])
        }
        
        #In bag probabilities output calculations
        for(k in 1:nrow(IN2)){
          if(t==1){
            t.in=which(IN2[k,1:ntree]!=0)
            votes.in= PRED[k,t.in]
            trees.in[k]=length(t.in)
            share.in[k]=length(which(votes.in=="LWR"))
          }else{
            t.in=which(IN2[k,(TREES[t-1]+1):ntree]!=0)
            votes.in= PRED[k,t.in]
            trees.in[k]= trees.in[k]+length(t.in)
            share.in[k]= share.in[k]+length(which(votes.in=="LWR"))
          }
        }
        prob.in=share.in/trees.in
        
        #try to find the best spliting point
        Opt_t<-opt_t(prob.in,d)
        
        
        #OOB probabilities output calculations
        prob.oob=rep(NA,nrow(IN2))
        
        for(k in 1:nrow(IN2)){
          if(t==1){
            t.oob=which(IN2[k,]==0)
            votes.oob[[k]]=PRED[k,t.oob]
            prob.oob[k]= length(which(votes.oob[[k]]=="LWR"))/length(votes.oob[[k]])
          }else{
            t.oob=which(IN2[k,(TREES[t-1]+1):TREES[t]]==0)
            votes.oob[[k]]=c(votes.oob[[k]],PRED[k,t.oob])
            prob.oob[k]= length(which(votes.oob[[k]]=="LWR"))/length(votes.oob[[k]])
          }
        }
        
        #error calculations
        tot_error[t,i]=error_TOT(prob.oob,test = d$type,opt_t = Opt_t)
        gcr_error[t,i]=error_GCR(prob.oob,test = d$type,opt_t = Opt_t)
        lwr_error[t,i]=error_LWR(prob.oob,test = d$type,opt_t = Opt_t)
        
      }
      
    }else{# without threshold
      tot_error[,i] <- rf$err.rate[TREES,1]
      gcr_error[,i] <- rf$err.rate[TREES,2]
      lwr_error[,i] <- rf$err.rate[TREES,3]
    }
    
  }
  
  tot_stat <- rbind(apply(as.matrix(tot_error),1,mean),
                    apply(as.matrix(tot_error),1,sd))
  gcr_stat <- rbind(apply(gcr_error,1,mean),
                    apply(gcr_error,1,sd))
  lwr_stat <- rbind(apply(lwr_error,1,mean),
                    apply(lwr_error,1,sd))

result <- list(rbind(tot_stat,gcr_stat,lwr_stat),param)

#======================
## TO CHANGE
save(result,file = paste("2- RESULTS/RF_output_",count,".RData",sep=""))
#======================
}
