## TUNING BOOSTING
## NICOLAS LANDRY  created the 15/12/2016
## LAST MODIFICATION: 11/01/2017
##OBJECTIVE: How to improve result from bo?


rm(list=ls())
setwd("H:\\BOOSTING\\")

#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
library(adabag)

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
alldata=read.csv(file="prisv11.csv",sep=";")
ids=which(alldata$type=="PWR"|alldata$type=="BWR")
levels(alldata$type)=c(levels(alldata$type),"LWR")
alldata$type[ids]="LWR"
ids.japan=which(alldata$country=="JAPAN" & alldata$year>2010)
alldata=alldata[-ids.japan,]

colnames=c("type","t11","t12","t13","t14","t15","t16","t17","t21","t31","t32","t33","t35","t41","t42","t43")
cnames=which(is.element(colnames(alldata),colnames))
countries=c("FRANCE","GERMANY","JAPAN","SPAIN","SWITZERLAND","UNITED KINGDOM","UNITED STATES OF AMERICA")
ids=which((alldata$type=="LWR"|alldata$type=="GCR") & alldata$country %in% countries)
d=alldata[ids,c(cnames)]
d$type=factor(d$type)
d=na.omit(d)

#Removal of zero year outages
S<-rep(0,nrow(d))
for(i in 1:nrow(d)){
  S[i]=(sum(d[i,2:(ncol(d)-1)]))
}
d<-d[-which(S==0),]
rm(S,i,ids,cnames,ids.japan,countries)

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
tunegrid <- expand.grid(.minsplit=seq(3,100,20),
                        .minbucket=seq(1,20,5),
                        .cp=c(0.1,0.2,0.3),
                        .tsetsize=seq(300,1100,100),
                        .ratio=0.5,
                        .coeflearn=c("Breiman","Freund"))
k <- nrow(tunegrid)


#### REMOVAL OF ALREADY COMPUTED RESULTS
# from simulation A : 20
SPLIT=1
idsA=seq((SPLIT-1)*j/6+1,SPLIT*j/6)[1:20]
# from simulation B : 19
SPLIT=2
idsB=seq((SPLIT-1)*j/6+1,SPLIT*j/6)[1:19]
# from simulation C : 1
SPLIT=3
idsC=seq((SPLIT-1)*j/6+1,SPLIT*j/6)[1:1]
# from simulation D : 42
SPLIT=4
idsD=seq((SPLIT-1)*j/6+1,SPLIT*j/6)[1:42]

ids.already=c(idsA,idsB,idsC,idsD)
rm(idsA,idsB,idsC,idsD,SPLIT)

result<-list()

for(count in 1:k){
  if(!(i %in% ids.already))
  rep     <- tunegrid[count,]
  
  control <- rpart.control(minsplit = rep$.minsplit,
                           minbucket = rep$.minbucket,
                           cp = rep$.cp)
  
  oob_error <- matrix(NA, nrow=5,ncol=30)
  gcr_error <- matrix(NA, nrow=5,ncol=30)
  lwr_error <- matrix(NA, nrow=5,ncol=30)
  
  for (i in 1:30){
    print(paste(count,",",i))
    error=1
    while(error==1){
      ids.train=sample(1:nrow(d),size = rep$.tsetsize,replace = F)
      if(length(which(d$type[ids.train]=="GCR"))>0) error =0
    }
    print("GCRs in the training set")
    ids.train.up=ids.bootstrap.up(ids.train,d$type[ids.train],ratioGT = rep$.ratio)
    test.set=d[-ids.train.up,]
    train.set=d[ids.train.up,]
    bo <-boosting(type~., data=train.set,mfinal=50,coeflearn = rep$.coeflearn,control = control)
    for(s in 1:5){
      pred=predict(bo,test.set,type = "class",newmfinal = 10*s)
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
  
  #======================
  ## TO CHANGE
  save(result,file = paste("A/boosting_output2_",count,".RData",sep=""))
  #======================
}
