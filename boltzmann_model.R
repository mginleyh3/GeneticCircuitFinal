params = c(k1,k2,k3,krnap,w12,w13,w23,w1r,w2r,w3r)
exper = data.frame(C1=c(1,1,0,0,1,1,0), C2=c(1,0,1,0,1,0,1),C3=c(1,0,0,1,0,1,1))
C12 = exper$C1*exper$C2
C13 = exper$C1*exper$C3
C23 = exper$C2*exper$C3
inter = cbind(C12,C13,C23)

thermo <- function(params,exp,inter){
    q<-params[1:3]
    weight <- params
    return(exp(-sum(q*exp)-sum(weight*inter)))
}

p <- function(weights){
  return(sum(weights[1:8])/sum(weights))
}

do.call(thermo,as.list)
thermo(c(1,1,1),exper[4,],inter[4,])
