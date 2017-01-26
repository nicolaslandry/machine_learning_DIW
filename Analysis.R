## BO Analysis
## NICOLAS LANDRY  created the 19/12/2016
## LAST MODIFICATION: 12/01/2017
## OBJECTIVE: visualize BO tuning?

#=====================================================
#================= 1 LOAD OF FILE ====================
#=====================================================
rm(list=ls())
error=TRUE
while(error){
  algo <- readline(prompt="Choose algorithm (CT,RF,BO,LOGIT): ")
  error= !(algo %in% c("CT","RF","BO","LOGIT"))
}
rm(error)
if(algo == "CT") folder="CLASSIFICATION"
if(algo == "RF") folder="RANDOM FOREST"
if(algo == "BO") folder="BOOSTING"
if(algo == "LOGIT") folder="LOGIT MODEL"

dir.create(paste("H:\\",folder,"\\4- ANALYSIS",sep=""),showWarnings = FALSE)
setwd(paste("H:\\",folder,"\\4- ANALYSIS",sep=""))
load(paste("H:\\",folder,"\\3- RESULTS TREATMENT/",algo,"_result_concatenated.RData",sep=""))

table=result_concatenated
rm(result_concatenated)
#We computed error rate not sensitivity and specificity
table$Sens=1-table$Sens
table$Spec=1-table$Spec

PARAMETERS<-colnames(table)[1:(ncol(table)-6)]
#=====================================================
#=================== 2 FUNCTIONS =====================
#=====================================================

ROCplot<-function(table, col,leg, leg.col,leg.title,xlim=c(0,100),ylim=c(0,100),leg.x="bottomleft"){
  plot(x = 100*table$Spec,y=100*table$Sens,col=col,ylim=ylim,xlim=xlim,pch=1,xlab="Specificity (in %)",ylab="Sensitivity (in %)")
  grid()
  abline(a=0,b = 1)
  legend(x=leg.x,legend = leg,col = leg.col,lwd = 3,title = leg.title)
}

ROCplotSD<-function(table, col,leg, leg.col,leg.title,xlim=c(0,100),ylim=c(0,100),leg.x="bottomleft"){
  plot(x = 100*table$Spec.sd,y=100*table$Sens.sd,col=col,ylim=ylim,xlim=xlim,pch=1,xlab="Specificity (in %)",ylab="Sensitivity (in %)")
  grid()
  abline(a=0,b = 1)
  legend(x=leg.x,legend = leg,col = leg.col,lwd = 3,title = leg.title)
}

PARplot<-function(table, param, xlab="", col,leg, leg.col,leg.title,ylim=c(0,100)){
  plot(x = param,y=100*table$Spec,col=col,ylim=ylim,pch=1,xlab="",ylab="correctness of the prediction (in %)")
  par(new=TRUE)
  plot(x = param,y=100*table$Sens,col=col,ylim=ylim,pch=2,xlab=xlab,ylab="")
  axis(side = 2,at = seq(0,100,10))
  grid()
  legend(x="bottomright",legend = leg,col = leg.col,lwd = 3,title = leg.title)
}

colors.choice<-function(table,parameter){
  p <-which(colnames(table)==parameter)
  col=rep(0,nrow(table))
  Param=unique(table[,p])
  colleg=rep(0,length(Param))
  for(t in 1:length(Param)){
    id=which(table[,p]==Param[t])
    col[id]=hsv(t/length(Param),1,1)
    colleg[t]=hsv(t/length(Param),1,1)
  }
  list(col,colleg,Param)
}

#=====================================================
#============= 3  DIRECTORIES ========================
#=====================================================
dir.create(paste("PNG"),showWarnings = FALSE)

for(par in PARAMETERS){
  ## DIRECTORY CREATION
  print(par)
  dir.create(paste("PNG/",par,sep = ""),showWarnings = FALSE)
  
  ## COLOR CHOICE
  c=which(colnames(table)==par)
  table<-table[order(table[,c]),]
  PLOTPAR<-colors.choice(table,par)
  
  ## GLOBAL PLOT
  png(filename = paste("PNG/",par,"/ROC_plot_",par,".png",sep=""),width = 1000,height = 1000,units = "px")
  ROCplot(table,PLOTPAR[[1]],leg=PLOTPAR[[3]],leg.col = PLOTPAR[[2]],leg.title = par)
  dev.off()
  
  ## PLOT PER VALUE
  for(p in PLOTPAR[[3]]){
    ids=which(table[,c]==p)
    # means
    png(filename = paste("PNG/",par,"/ROC_plot_",par,"=",p,".png",sep = ""),width = 1000,height = 1000,units = "px")
    ROCplot(table[ids,],PLOTPAR[[1]][ids],leg=PLOTPAR[[3]],leg.col =  PLOTPAR[[2]],leg.title = par)
    dev.off()
    # standard deviation
    png(filename = paste("PNG/",par,"/ROCSD_plot_",par,"=",p,".png",sep = ""),width = 1000,height = 1000,units = "px")
    ROCplotSD(table[ids,],PLOTPAR[[1]][ids],leg=PLOTPAR[[3]],leg.col = PLOTPAR[[2]],leg.title = par,leg.x = "bottomright")
    dev.off()
  }
}

ids<-which(abs(table$Sens-table$Spec)<min(table$Sens.sd,table$Spec.sd))
table2=table[ids,]
