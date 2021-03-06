#you probably have to install these packages
library(optimr)
library(ggplot2)

data <- read.csv("espndata.csv")
data <- data[complete.cases(data),]
param = c(k1=-.1,k2=-.1,k3=-.1,w12=-.1,w13=-.1,w23=-.1,wRNAP=-.1,w1r=-.1,w2r=-.1,w3r=-.1)

#creates experiment matrix of σs
exper = data.frame(C1=c(0,1,1,0,0,1,1,0), C2=c(0,1,0,1,0,1,0,1),C3=c(0,1,0,0,1,0,1,1))
exper$C12 = exper$C1*exper$C2
exper$C13 = exper$C1*exper$C3
exper$C23 = exper$C2*exper$C3
exper=rbind(exper,exper)
exper$RNAP = c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0)
exper$R1 = exper$RNAP*exper$C1
exper$R2 = exper$RNAP*exper$C2 
exper$R3 = exper$RNAP*exper$C3 

p_rnap = rep(0,8)

to_optim <- function(params){
  #possible states for each experiment (without RNAP bound is numbers + 8
  p1 <- c(1)
  p2 <- c(1:8)
  p3 <- c(1,3)
  p4 <- c(1,4)
  p5 <- c(1,5)
  p6 <- c(1,3,4,6)
  p7 <- c(1,3,5,7)
  p8 <- c(1,4,5,8)

  ## calculates probability RNAP is bound in each experiment
  p_rnap[1] <- sum(thermo(params)[p1])/sum(thermo(params)[c(p1,p1+8)])
  p_rnap[2] <- sum(thermo(params)[p2])/sum(thermo(params)[c(p2,p2+8)])
  p_rnap[3] <- sum(thermo(params)[p3])/sum(thermo(params)[c(p3,p3+8)])
  p_rnap[4] <- sum(thermo(params)[p4])/sum(thermo(params)[c(p4,p4+8)])
  p_rnap[5] <- sum(thermo(params)[p5])/sum(thermo(params)[c(p5,p5+8)])
  p_rnap[6] <- sum(thermo(params)[p6])/sum(thermo(params)[c(p6,p6+8)])
  p_rnap[7] <- sum(thermo(params)[p7])/sum(thermo(params)[c(p7,p7+8)])
  p_rnap[8] <- sum(thermo(params)[p8])/sum(thermo(params)[c(p8,p8+8)])
  
  corrr = cor(p_rnap,data[,2],method="pearson")
  if(is.numeric(corrr)){
    return(1-corrr)
  }
  else{
    return(1)
    }
}
##calculates the ∆G of each state
thermo <- function(params){
  weight_vec <- exp(-rowSums(t(params*t(exper)))/.5961)
  return(weight_vec)
}

control <- list(trace=0)
##iterations of optimization
for(i in 1:50){
  print(i)
  parameters <- runif(10,min=-1,max=1)
  df<-t(as.data.frame(parameters))  
  colnames(df)<-labels(param)
  filename <- paste("~/Documents/Research/thermoModel/mmp17_params",i,sep="_")
  optimized <- opm(parameters,fn=to_optim, method="ALL",control = control,lower=-5,upper=5)
  optimized <- rbind(optimized,"initial"=append(parameters,rep(NA,7)))
  write.table(format(optimized, digits=2),filename,quote=F,sep='\t')
}

#run this line for a once-through of the optimization
optimized <- opm(param,fn=to_optim,control=control, method="ALL",lower=-5,upper=5)
write.table(format(optimized, digits=2),"~/Documents/Research/thermoModel/thermoModel_params0.txt",quote=F,sep="\t",upper=20,lower=-20)


