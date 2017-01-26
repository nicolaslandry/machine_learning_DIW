## TUNING BOOSTING
## NICOLAS LANDRY  created the 15/12/2016
## LAST MODIFICATION: 15/12/2016
##OBJECTIVE: How to improve result from bo?


rm(list=ls())
setwd("H:\\Machine_Learning\\")
setwd("/projekte/sseifert/homes/Machine_Learning")
#=====================================================
#================= 1 LIBRARIES LOAD ==================
#=====================================================
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

boosting<-function (formula, data, boos = TRUE, mfinal = 100, coeflearn = "Breiman",control, ...) {
  if (!(as.character(coeflearn) %in% c("Freund", "Breiman", 
                                       "Zhu"))) {
    stop("coeflearn must be 'Freund', 'Breiman' or 'Zhu' ")
  }
  formula <- as.formula(formula)
  vardep <- data[, as.character(formula[[2]])]
  n <- length(data[, 1])
  nclases <- nlevels(vardep)
  pesos <- rep(1/n, n)
  guardarpesos <- array(0, c(n, mfinal))
  w <- rep(1/n, n)
  data <- data.frame(pesos, data)
  arboles <- list()
  pond <- rep(0, mfinal)
  pred <- data.frame(rep(0, n))
  arboles[[1]] <- rpart(formula, data = data[, -1], control = rpart.control(minsplit = 1, 
                                                                            cp = -1, maxdepth = 30))
  nvar <- dim(varImp(arboles[[1]], surrogates = FALSE, competes = FALSE))[1]
  imp <- array(0, c(mfinal, nvar))
  for (m in 1:mfinal) {
    if (boos == TRUE) {
      k <- 1
      while (k == 1) {
        boostrap <- sample(1:n, replace = TRUE, prob = pesos)
        fit <- rpart(formula, data = data[boostrap, -1], 
                     control = control)
        k <- length(fit$frame$var)
      }
      flearn <- predict(fit, newdata = data[, -1], type = "class")
      ind <- as.numeric(vardep != flearn)
      err <- sum(pesos * ind)
    }
    if (boos == FALSE) {
      w <<- pesos
      fit <- rpart(formula = formula, data = data[, -1], 
                   weights = w, control = control)
      flearn <- predict(fit, data = data[, -1], type = "class")
      ind <- as.numeric(vardep != flearn)
      err <- sum(pesos * ind)
    }
    c <- log((1 - err)/err)
    if (coeflearn == "Breiman") {
      c <- (1/2) * c
    }
    if (coeflearn == "Zhu") {
      c <- c + log(nclases - 1)
    }
    guardarpesos[, m] <- pesos
    pesos <- pesos * exp(c * ind)
    pesos <- pesos/sum(pesos)
    maxerror <- 0.5
    eac <- 0.001
    if (coeflearn == "Zhu") {
      maxerror <- 1 - 1/nclases
    }
    if (err >= maxerror) {
      pesos <- rep(1/n, n)
      maxerror <- maxerror - eac
      c <- log((1 - maxerror)/maxerror)
      if (coeflearn == "Breiman") {
        c <- (1/2) * c
      }
      if (coeflearn == "Zhu") {
        c <- c + log(nclases - 1)
      }
    }
    if (err == 0) {
      pesos <- rep(1/n, n)
      c <- log((1 - eac)/eac)
      if (coeflearn == "Breiman") {
        c <- (1/2) * c
      }
      if (coeflearn == "Zhu") {
        c <- c + log(nclases - 1)
      }
    }
    arboles[[m]] <- fit
    pond[m] <- c
    if (m == 1) {
      pred <- flearn
    }
    else {
      pred <- data.frame(pred, flearn)
    }
    if (length(fit$frame$var) > 1) {
      k <- varImp(fit, surrogates = FALSE, competes = FALSE)
      imp[m, ] <- k[sort(row.names(k)), ]
    }
    else {
      imp[m, ] <- rep(0, nvar)
    }
  }
  classfinal <- array(0, c(n, nlevels(vardep)))
  for (i in 1:nlevels(vardep)) {
    classfinal[, i] <- matrix(as.numeric(pred == levels(vardep)[i]), 
                              nrow = n) %*% as.vector(pond)
  }
  predclass <- rep("O", n)
  predclass[] <- apply(classfinal, 1, FUN = select, vardep.summary = summary(vardep))
  imppond <- as.vector(as.vector(pond) %*% imp)
  imppond <- imppond/sum(imppond) * 100
  names(imppond) <- sort(row.names(k))
  votosporc <- classfinal/apply(classfinal, 1, sum)
  ans <- list(formula = formula, trees = arboles, weights = pond, 
              votes = classfinal, prob = votosporc, class = predclass, 
              importance = imppond)
  attr(ans, "vardep.summary") <- summary(vardep, maxsum = 700)
  mf <- model.frame(formula = formula, data = data)
  terms <- attr(mf, "terms")
  ans$terms <- terms
  ans$call <- match.call()
  class(ans) <- "boosting"
  ans
}

#=====================================================
#=============== 4 LOAD OF THE DATASET ===============
#=====================================================
cat("load of the dataset \n")
alldata=read.csv(file="Pris_data_all_with_outages_v05_nl.csv",sep=";")
ids=which(alldata$type=="PWR"|alldata$type=="BWR")
levels(alldata$type)=c(levels(alldata$type),"LWR")
alldata$type[ids]="LWR"
ids.japan=which(alldata$country=="JAPAN" & alldata$year>2010)
alldata=alldata[-ids.japan,]

colnames=c("type","t11","t12","t13","t14","t15","t16","t17","t21","t31","t32","t33","t35","t41","t42","t43")
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
rm(ids.japan)
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

#### WITH FOREACH LOOP
tunegrid <- expand.grid(.minsplit=seq(3,100,20),
                        .minbucket=seq(1,20,5),
                        .cp=c(0.001,0.01,0.1),
                        .tsetsize=seq(100,1100,100),
                        .ratio=0.5,
                        .coeflearn=c("Breiman","Freund"))
j <- nrow(tunegrid)



sim <- foreach(count=1:j, .packages="randomForest")%dopar%{
  
  rep     <- tunegrid[count,]
  
  control <- rpart.control(minsplit = rep$.minsplit,
                           minbucket = rep$.minbucket,
                           cp = rep$.cp)
  
  oob_error <- matrix(NA, nrow=5,ncol=30)
  gcr_error <- matrix(NA, nrow=5,ncol=30)
  lwr_error <- matrix(NA, nrow=5,ncol=30)
  
  for (i in 1:30){
    ids.train=sample(1:nrow(d),size = rep$.tsetsize,replace = F)
    ids.train.up=ids.bootstrap.up(ids.train,d$type[ids.train],ratioGT = rep$.ratio)
    test.set=d[-ids.train.up,]
    train.set=d[ids.train.up,]
    bo <-boosting(type~., data=train.set,mfinal=100,coeflearn = rep$.coeflearn,control = control)
    for(s in 1:5){
      pred=predict(bo,test.set,type = "class",newmfinal = 20*s)
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
  
  rbind(oob_list,gcr_list,lwr_list)
}

save(sim, "simulation_output.RData")

#SAVING
#write.table(rf_gridsearch$results,"result_tuning_rf.txt")





