## TUNING CLASSIFICATION TREE
## NICOLAS LANDRY  created the 15/12/2016
## LAST MODIFICATION: 15/12/2016
##OBJECTIVE: How to improve result from ct?


rm(list=ls())
setwd("H:\\Machine_Learning\\")
setwd("/projekte/sseifert/homes/Machine_Learning")
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(rpart)
library(mlbench)
library(doParallel)

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
  print(idsG2)
  ids2<-c(idsG2,idsL2)
  #print(length(idsG2)/length(ids2))
  ids2
}

#=====================================================
#=============== 4 LOAD OF THE DATASET ===============
#=====================================================
cat("load of the dataset \n")
alldata=read.csv(file="Pris_data_all_with_outages_v05_nl.csv",sep=";")
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

#PARALLEL PROCESSING
max_cores <- detectCores()[1]
max_cores
if (max_cores > 25) max_cores <- 7
cl <- makeCluster(max_cores, homogeneous=T)
registerDoParallel(cl)

seed=7
set.seed(seed)

#### WITH FOREACH LOOP
nGCR=length(which(d$type=="GCR"))
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
rm(tunegrid.do,tunegrid.up)
k <- nrow(tunegrid)


sim <- foreach(count=1:nrow(tunegrid), .packages=c("randomForest","rpart"))%dopar%{
    rep     <- tunegrid[count,]

    
    control <- rpart.control(minsplit = rep$.minsplit,
                          minbucket = rep$.minbucket,
                          cp = rep$.cp)
    
    oob_error <- matrix(NA, nrow=30,ncol=1)
    gcr_error <- matrix(NA, nrow=30,ncol=1)
    lwr_error <- matrix(NA, nrow=30,ncol=1)

    for (i in 1:30){
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
        pred=predict(ct,test.set,type = "class")
        
        oob_error[i] <- length(which((pred=="LWR" & test.set$type=="GCR")|(pred=="GCR" & test.set$type=="LWR")))/length(pred)
        gcr_error[i] <- length(which(pred=="LWR" & test.set$type=="GCR"))/length(which(test.set$type=="GCR"))
        lwr_error[i] <- length(which(pred=="GCR" & test.set$type=="LWR"))/length(which(test.set$type=="LWR"))
    }


    oob_list <- rbind(mean(oob_error),sd(oob_error))
    gcr_list <- rbind(mean(gcr_error),sd(gcr_error))
    lwr_list <- rbind(mean(lwr_error),sd(lwr_error))
    
    rbind(oob_list,gcr_list,lwr_list)
}
save(sim, file="simulation_output_ct.RData")
#SAVING
#write.table(rf_gridsearch$results,"result_tuning_rf.txt")
