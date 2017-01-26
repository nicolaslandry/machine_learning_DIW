## TUNING RANDOM FOREST
## NICOLAS LANDRY  created the 23/11/2016
## LAST MODIFICATION: 11/01/2017
##OBJECTIVE: How to improve result from rf?


rm(list=ls())
setwd("H:\\Machine_Learning\\")
setwd("/projekte/sseifert/homes/Machine_Learning")
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

#=====================================================
#=============== 4 LOAD OF THE DATASET ===============
#=====================================================
cat("load of the dataset \n")
alldata=read.csv(file="Pris_data_all_with_outages_v11_nl.csv",sep=";")
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
rm(s,S,i,ids,cnames,ids.japan,countries)

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

#VARIABLES
#number=10
#repeats=3
#tunegrid <- expand.grid(.ntree=seq(200,900,100),.mtry=seq(1,5,1),.classwt=seq(1,248,4),.maxnodes=seq(2,18,2),.sampsize=seq(50,1100,80))
#iter=number*repeats*nrow(tunegrid)
#num_rs=number*repeats

# SET SEEDS FOR TRAIN AND PUT THEM IN CONTROL PARAMETERS
seeds <- sample.int(n = 10000000L, size = iter + 1L)
seeds <- lapply(seq(from = 1L, to = length(seeds), by = nrow(tunegrid)),
                function(x) { seeds[x:(x+nrow(tunegrid)-1L)] })
seeds[[num_rs + 1L]] <- seeds[[num_rs + 1L]][1L]


#control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid",classProbs = TRUE,summaryFunction=twoClassSummary,seeds = seeds)

#LAST PARAMETERS
compteur=0
metric <- "ROC"

# PROCESSING
#rf_gridsearch <- train(type~., data=d, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)


#### WITH FOREACH LOOP

tunegrid <- expand.grid(.ntree=1800,.mtry=seq(1,5,1),.classwt=seq(1,248,4),.maxnodes=seq(2,18,2),.sampsize=seq(50,1100,80))
j <- nrow(tunegrid)

sim <- foreach(count=1:j, .packages="randomForest")%dopar%{
 
    rep <- tunegrid[count,]
    
    oob_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
    gcr_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
    lwr_error <- matrix(NA, nrow=rep$.ntree,ncol=30)
    for (i in 1:30){
        rf <- randomForest(type~., data=d, 
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
    
    rbind(oob_list,gcr_list,lwr_list)

}
save(sim, "simulation_output.RData")

#SAVING
#write.table(rf_gridsearch$results,"result_tuning_rf.txt")
