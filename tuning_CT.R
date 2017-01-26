## TUNING CLASSIFICATION TREE
## NICOLAS LANDRY  created the 15/12/2016
## LAST MODIFICATION: 12/01/2017
##OBJECTIVE: How to improve result from ct?


rm(list=ls())

PARALLEL=FALSE
THRESHOLD=TRUE
#setwd("H:\\Machine_Learning\\")
#setwd("/projekte/sseifert/homes/Machine_Learning")

setwd("H:\\CLASSIFICATION\\")
dir.create("2- RESULTS",showWarnings = FALSE)
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(rpart)
library(pROC)

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

tunegrid.up <- expand.grid(.minsplit=seq(3,40,2),
                           .minbucket=seq(1,20,2),
                           .cp=c(0.00001,0.0001,0.001,0.01,0.1),
                           .tsetsize=seq(100,1100,80),
                           .ratio=seq(0.1,0.8,0.1),
                           .up=TRUE)
tunegrid.do <- expand.grid(.minsplit=seq(3,40,2),
                           .minbucket=seq(1,20,2),
                           .cp=c(0.00001,0.0001,0.001,0.01,0.1),
                           .tsetsize=seq(10,200,20),
                           .ratio=0.5,
                           .up=FALSE)
tunegrid=rbind(tunegrid.up,tunegrid.do)
k <- nrow(tunegrid)

rm(tunegrid.do,tunegrid.up)



#foreach(count=1:k, .packages=c("randomForest","rpart"))%dopar%{
for(count in 25:k){
  print(count)
  result<-list()
  param     <- tunegrid[count,]
  
  control <- rpart.control(minsplit = param$.minsplit,
                           minbucket = param$.minbucket,
                           cp = param$.cp)
  
  ntrees=c(1)
  list2env(error_matrices(length(ntrees),30,ntrees),env=environment())   #error matrices
  
  for (i in 1:30){

    sets<-train_test_sets(d,param) # train and test sets

    ct <- rpart(type~., data=sets$train, control = control)

    # optimal threshold to split
    Opt_t=NA
    if(THRESHOLD){
      pred=predict(ct,sets$train,type = "prob")
      Opt_t<-opt_t(pred[,1],sets$train)
      
      pred=predict(ct,sets$test,type = "prob")
      pred=pred[,2]
    } else{
      pred=predict(ct,sets$test,type = "class")
    }

    tot_error[1,i] <- error_TOT(pred,sets$test$type,Opt_t)
    gcr_error[1,i] <- error_GCR(pred,sets$test$type,Opt_t)
    lwr_error[1,i] <- error_LWR(pred,sets$test$type,Opt_t)

  }
  
  tot_stat <- error_stat(tot_error)
  gcr_stat <- error_stat(gcr_error)
  lwr_stat <- error_stat(lwr_error)
  
  result <- list(rbind(tot_stat,gcr_stat,lwr_stat),param)
  
  #======================
  ## TO CHANGE
  save(result,file = paste("2- RESULTS/CT_output_",count,".RData",sep=""))
  #======================
}
