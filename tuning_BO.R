## TUNING BOOSTING
## NICOLAS LANDRY  created the 15/12/2016
## LAST MODIFICATION: 12/01/2017
##OBJECTIVE: How to improve result from bo?


rm(list=ls())
#setwd("H:\\Machine_Learning\\")
#setwd("/projekte/sseifert/homes/Machine_Learning")

setwd("H:\\BOOSTING\\")
#lib <- .libPaths()
#.libPaths("/projekte/sseifert/homes/Machine_Learning/packages")
#install.packages("fastAdaboost",
#lib="/projekte/sseifert/homes/Machine_Learning",
#                 repos="http://cran.ms.unimelb.edu.au/",dependencies=TRUE)
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(adabag)
library(doParallel)
library(fastAdaboost)
library(rpart)

#=====================================================
#=============== 2 FUNCTIONS DECLARATION =============
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
#=============== 3 LOAD OF THE DATASET ===============
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
    }
  }else{
    for(c in 4:ncol(d)){
      d[r,c]=d[r,c]/(8760-s+d[r,c])
    }
  }
}
d<-d[,-c(1,3)]
rm(s,S,i,ids,cnames,ids.japan,countries)

#=====================================================
#================== 4 ANALYSIS =======================
#=====================================================

#PARALLEL PROCESSING
max_cores <- detectCores()[1]
max_cores
if (max_cores > 25) max_cores <- 7
cl <- makeCluster(max_cores, homogeneous=T)
registerDoParallel(cl)

seed=7
set.seed(seed)

tunegrid <- expand.grid(.minsplit=c(3,6,9),
                        .minbucket=seq(1,20,5),
                        .cp=c(0.01,0.05,0.1),
                        .tsetsize=c(300,600,900,1200),
                        .ratio=c(0.5,0.6),
                        .coeflearn=c("Breiman"),
                        .up=c("TRUE","FALSE"))
k <- nrow(tunegrid)







foreach(count=1:k, .packages=c("adabag","fastAdaboost","rpart"))%dopar%{
  result<-list()
  rep     <- tunegrid[count,]
  
  control <- rpart.control(minsplit = rep$.minsplit,
                           minbucket = rep$.minbucket,
                           cp = rep$.cp)
  
  oob_error <- matrix(NA, nrow=6,ncol=30); rownames(oob_error)=c("10trees","20trees","30trees","40trees","50trees","60trees")
  gcr_error <- matrix(NA, nrow=6,ncol=30); rownames(oob_error)=c("10trees","20trees","30trees","40trees","50trees","60trees")
  lwr_error <- matrix(NA, nrow=6,ncol=30); rownames(oob_error)=c("10trees","20trees","30trees","40trees","50trees","60trees")
  
  for (i in 1:30){
    print(paste(count,",",i))
    error=1
    while(error==1){
      ids.train=sample(1:nrow(d),size = rep$.tsetsize,replace = F)
      if(length(which(d$type[ids.train]=="GCR"))>0) error =0
    }
    print("GCRs in the training set")
    if(rep$.up){    
      ids.train.up=ids.bootstrap.up(ids.train,d$type[ids.train],ratioGT = rep$.ratio)
      test.set=d[-ids.train.up,]
      train.set=d[ids.train.up,]
    }else{
      ids.train.do=ids.bootstrap.do(d$type,ratioGT = rep$.ratio, size=rep$.tsetsize)
      test.set=d[-ids.train.do,]
      train.set=d[ids.train.do,]
    }
    bo <-boosting(type~., data=train.set,mfinal=60, coeflearn = rep$.coeflearn,control = control)
    for(s in 1:6){
      #bo <-adaboost(type~., data=train.set,nIter=10*s, coeflearn = rep$.coeflearn,control = control)
      #pred=predict(bo,test.set,type = "class")
      pred=predict(bo,test.set,newmfinal=10*s,type = "class")
      oob_error[s,i] <- length(which((pred$class=="LWR" & test.set$type=="GCR")|(pred$class=="GCR" & test.set$type=="LWR")))/length(pred$class)
      gcr_error[s,i] <- length(which(pred$class=="LWR" & test.set$type=="GCR"))/length(which(test.set$type=="GCR"))
      lwr_error[s,i] <- length(which(pred$class=="GCR" & test.set$type=="LWR"))/length(which(test.set$type=="LWR"))
    }
  }
  oob_list <- rbind(apply(as.matrix(oob_error),1,mean),
                    apply(as.matrix(oob_error),1,sd))
  gcr_list <- rbind(apply(gcr_error,1,mean),
                    apply(gcr_error,1,sd))
  lwr_list <- rbind(apply(lwr_error,1,mean),
                    apply(lwr_error,1,sd))
  
  result[[1]] <- rbind(oob_list,gcr_list,lwr_list)
  result[[2]] <- tunegrid[count,]
  
  #======================
  ## TO CHANGE
  save(result,file = paste("BO_output/boosting_output3_",count,".RData",sep=""))
  #======================
}
