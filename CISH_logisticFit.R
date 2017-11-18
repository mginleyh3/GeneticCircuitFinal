source("enhancerLib.R")
A <- read.csv("~/Documents/classes/genetic_circuits/superEnhancerCode/super-enhancer-code/scripts/cishData.csv")
temp<-structure(data.frame(C1=c(1,1,0,0,1,1,0), C2=c(1,0,1,0,1,0,1),C3=c(1,0,0,1,0,1,1), y=as.numeric(A[1:7,2])));
temp[,1:3]<-1*!temp[,1:3]
#Without interactions
test_design = cbind(temp$C1,temp$C2,temp$C3)
log.ln=deOptim.mod(response = temp$y,design.mat = test_design, ll.fun=logistic.model, parms = runif(6),lower = c(rep(-200,5),10^-2),upper = 200,error.mod="log-normal",refine=TRUE)
bic(log.ln,32)
pred.log=logistic.predict(design.mat = test_design,x = log.ln$par)
quartz(); plot(pred.log$expression, temp$y)

#With interactions
C12 = temp$C1*temp$C2
C13 = temp$C1*temp$C3
C23 = temp$C2*temp$C3
test_design_int = cbind(test_design,C12,C13,C23)
log.ln_int=deOptim.mod(response = temp$y,design.mat = test_design_int, ll.fun=logistic.model, parms = runif(9),lower = c(rep(-200,8),10^-2), upper = 200,error.mod="gaussian",refine=TRUE)
bic(log.ln_int,32)
pred.log_ln=logistic.predict(design.mat = test_design_int, x = log.ln_int$par)
quartz(); plot(pred.log$expression, temp$y)


plot(pred.log_ln$activity,pred.log$expression,add=TRUE,col="red",ylim=c(0,3),ylab="")
par(new=T)
plot(pred.log_ln$activity,A[,2],add=T,col="blue",ylim=c(0,3),ylab="expression")
curve(2.68/(1+exp(-91.3755*x)),0,250,add = T,col="red",ylim=c(0,3))


