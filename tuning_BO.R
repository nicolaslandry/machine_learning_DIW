## TUNING BOOSTING
## NICOLAS LANDRY  created the 15/12/2016
## LAST MODIFICATION: 12/01/2017
##OBJECTIVE: How to improve result from bo?


rm(list=ls())

PARALLEL=FALSE
#setwd("H:\\Machine_Learning\\BOOSTING")
#setwd("/projekte/sseifert/homes/Machine_Learning/BOOSTING")

setwd("H:\\BOOSTING\\")

dir.create("2- RESULTS",showWarnings = FALSE)

#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(adabag)
library(doParallel)
library(fastAdaboost)
library(rpart)
library(bst)
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

#TUNEGRID
tunegrid.up <- expand.grid(.cp=c(0.1,0.01,0.001),
                           .tsetsize=c(300,600,900,1200),
                           .ratio=seq(0.2,0.6,0.1),
                           .coeflearn=c("Breiman"),
                           .up=c(TRUE))
tunegrid.do <- expand.grid(.cp=c(0.1,0.01,0.001),
                           .tsetsize=c(50,75,100,125,150,200),
                           .ratio=seq(0.2,0.6,0.1),
                           .coeflearn=c("Breiman"),
                           .up=c(FALSE))

tunegrid.do <- tunegrid.do[(tunegrid.do$.ratio*tunegrid.do$.tsetsize)<sum(d$type=="GCR"),]
tunegrid <- rbind(tunegrid.up,tunegrid.do)
k <- nrow(tunegrid)

rm(tunegrid.do,tunegrid.up)


#foreach(count=1:k, .packages=c("adabag","fastAdaboost","rpart"))%dopar%{
for(count in 18:k)  {
  param     <- tunegrid[count,]
  
  control <- rpart.control(minsplit = param$.minsplit,
                           minbucket = param$.minbucket,
                           cp = param$.cp)
  ntrees=seq(5,80,5)
  list2env(error_matrices(length(ntrees),30,ntrees),env=environment())   #error matrices
  
  for (i in 1:30){
    sets<-train_test_sets(d,param) # train and test sets
    
    #BOOSTING
    #bo <-boosting(type~., data=train.set,mfinal=60, coeflearn = param$.coeflearn,control = control)
    
    for(s in 1:length(ntrees)){
      print(paste(count,",",i,s))
      #ADABOOST
      #bo <-adaboost(type~., data=sets$train,nIter=5*s, coeflearn = param$.coeflearn,control = control)
      #pred=predict(bo,sets$test,type = "class")
      
      #pred=predict(bo,test.set,newmfinal=5*s,type = "class")$class
      
      #BST
      x=as.matrix(sets$train[,-1])
      y=rep(0,length(sets$train[,1]))
      y[which(sets$train[,1]=="GCR")]<-  1
      y[which(sets$train[,1]!="GCR")]<- -1
      
      bo2 <- bst(x = x,y = y,cost = 1/2,family="hinge",ctrl = bst_control(mstop = 5*s,nu=1),control.tree = control,learner="tree")
      pred2 <-predict(bo2,sets$test,type= "class")
      pred2[which(pred2==1)] = "GCR"
      pred2[which(pred2==-1)]= "LWR"
      
      tot_error[s,i] <- error_TOT(pred2,sets$test$type)
      gcr_error[s,i] <- error_GCR(pred2,sets$test$type)
      lwr_error[s,i] <- error_LWR(pred2,sets$test$type)
      
    }
  }
  tot_stat <- error_stat(tot_error)
  gcr_stat <- error_stat(gcr_error)
  lwr_stat <- error_stat(lwr_error)

  result <- list(rbind(tot_stat,gcr_stat,lwr_stat),param)

  
  #======================
  ## TO CHANGE
  save(result,file = paste("2- RESULTS/BO_output_",count,".RData",sep=""))
  #======================
}
