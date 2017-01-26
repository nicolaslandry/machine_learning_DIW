## CONCATENATE RESULTS BO
## NICOLAS LANDRY  created the 12/01/2017
## LAST MODIFICATION: 13/01/2017
##OBJECTIVE: Concatenate results CT

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

setwd(paste("H:\\",folder,sep=""))
dir.create("3- RESULTS TREATMENT",showWarnings = FALSE)

#=====================================================
#===== 1 CREATION OF A LIST WITH ALL RESULTS =========
#=====================================================
Ncomb=length(list.files("2- RESULTS/"))
list.results=list()
for(i in 1:Ncomb){
  print(i)
  load(paste("2- RESULTS/",algo,"_output_",i,".RData",sep = ""))
  if(algo=="CT"){
    list.results[[i]]<- c(result[[2]],t(result[[1]]))
  }else if(algo=="RF")  {
    temp=result[[2]][rep(1,ncol(result[[1]])),]
    ntree=seq(200,1800,100)
    list.results[[i]]<- cbind(temp,ntree,t(result[[1]]))
  }else if(algo=="BO")  {
    #temp=result[[2]][rep(1,ncol(result[[1]])),]
    temp=result[[2]][rep(1,ncol(result[[3]])),]
    #ntree=c(40,50,60)
    ntree=seq(5,60,5)
    #list.results[[i]]<- cbind(temp,ntree,t(result[[1]]))
    list.results[[i]]<- cbind(temp,ntree,t(result[[3]]))
  }else if(algo=="LOGIT"){
    list.results[[i]]<- c(result[[2]],t(result[[1]]))
  }
}

save(list.results,file="3- RESULTS TREATMENT/list.results.RData")
rm(result,i,ntree,temp)
list.results[[1]]
#=====================================================
#=== 2 TRANSFORMATION OF THE LIST INTO A MATRIX ======
#=====================================================
# Concatenation
end=FALSE
list.temp=list.results
while(!end){
  l=length(list.temp)
  list.temp2=list()
  for(i in 1:((l+1)/2)){
    print(paste(l,i))
    if(2*i<=l){
      list.temp2[[i]]=rbind(list.temp[[2*i-1]],list.temp[[2*i]])
    }else{
      list.temp2[[i]]=rbind(list.temp[[2*i-1]])
    }
    
  }
  list.temp=list.temp2
  if(length(list.temp)==1) end=TRUE
}
result_concatenated=list.temp[[1]]

if(algo == "CT"){
  colnames <- c("minsplit","minbucket","cp","tsetsize","ratio","up","OOB.mean","OOB.sd","Sens","Sens.sd","Spec","Spec.sd")
  result_concatenated<-as.data.frame(matrix(unlist(result_concatenated),ncol = 12 ))
} 
if(algo == "RF"){
  result_concatenated<-result_concatenated[,-1]
  colnames <- c("mtry","classwt","maxnodes","sampsize","up","ntree","OOB.mean","OOB.sd","Sens","Sens.sd","Spec","Spec.sd")
} 
if(algo == "BO") colnames <- c("tsetsize","ratio","coeflearn","up","ntree","OOB.mean","OOB.sd","Sens","Sens.sd","Spec","Spec.sd")
if(algo == "LOGIT"){
  colnames <- c("ratioGT","sampsize","up","OOB.mean","OOB.sd","Sens","Sens.sd","Spec","Spec.sd")
  result_concatenated<-as.data.frame(matrix(unlist(result_concatenated),ncol = 9 ))
} 
colnames(result_concatenated)<-colnames

rm(list.temp,list.temp2,end,i,l)

file=paste("3- RESULTS TREATMENT/",algo,"_result_concatenated.RData",sep="")
save(result_concatenated,file=file)

## OPTIMAL T ADDITION IN THE CASE OF LOGIT MODEL

if(algo=="LOGIT"){
  Ncomb=length(list.files("2- RESULTS/"))
  
  list.results=list()
  opt_t=rep(0,30)
  for(i in 1:Ncomb){
    print(i)
    load(paste("2- RESULTS/",algo,"_output_",i,".RData",sep = ""))
    opt_t=rbind(opt_t,result[[3]])
  }
  colnames(opt_t)=sprintf("opt_t[%s]",seq(1:30))
  opt_t=opt_t[-1,]
  result_concatenated_with_optT<-cbind(opt_t,result_concatenated)
  file=paste("3- RESULTS TREATMENT/",algo,"_result_concatenated_with_otpT.csv",sep="")
  write.csv2(result_concatenated_with_optT,file=file)
}
