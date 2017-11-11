rm(list=ls())
setwd("~/Desktop/347 GLM/HW 6")
load("mnist.rdata")
m = matrix(as.numeric(TE[2,]),nrow=28,byrow=F)
m[m!=0] = 1
m = apply(m,1,rev)
image(t(m),axes=F,col=grey(seq(0,1,len=256)))

##################

library("nnet")
# colnames(dat) = paste0('X',1:784)
dig1 = multinom(ytr~.,data=TR,MaxNWt=10000)
fit1 = predict(dig1,newdata=TE,type="class")
fit2 = predict(dig2,newdata=TE,type="class")
fito = predict(dig1,newdata=TR,type="class")
sum(fit1!=yte)/length(yte)

ranboost = function(TR,ytr,ratep,raten,M,TE,yte){
  p = ncol(TR)
  n = nrow(TR)
  pp = floor(ratep*p)
  nn = floor(raten*n)
  w = rep(1,n)
  err = rep(0,M)
  alpha = rep(0,M)
  resmat = matrix(0,nrow(TE),10)
  testerr = rep(0,M)
  for(i in 1:M){
    subp = sample(1:p,pp,replace = FALSE)
    subn = sample(1:n,prob=w,replace=TRUE)
    digboost = multinom(ytr[subn]~.,data=TR[subn,subp],MaxNWt=10000)
    fitboost = predict(digboost,newdata=TR,type="class")
    mislab = which(fitboost!=ytr)
    err[i] = sum(w[mislab])/sum(w)
    alpha[i] = log((1-err[i])/err[i]) #+log(9)
    w[mislab] = w[mislab]*exp(alpha[i])
    tmp = predict(digboost,newdata=TE,type="prob")
    resmat = resmat + alpha[i]*tmp
    fittest = apply(resmat,1,which.max)-1
    testerr[i] = sum(fittest!=yte)/length(yte)
  }
  return(list(trainerr=err,testerr=testerr,fittest = fittest))
}

res = ranboost(TR,ytr,ratep=0.5,raten=0.5,M=10,TE,yte)
record = list()
record[[1]] = res # 0.3, 0.5
record[[2]] = res # 0.2, 0.5
record[[3]] = res # 0.3, 0.3
record[[4]] = res # 0.3, 0.2
record[[5]] = res # 0.5, 0.5

library(ggplot2)
ggplot(data.frame(x=1:10,terr=record[[1]]$testerr,err=record[[1]]$trainerr),aes(x,terr)) +
  geom_line() + geom_point() + geom_line(aes(x,err),color="red") + geom_point(aes(x,err),color="red")+
  scale_x_continuous(breaks=1:10) #+ scale_y_continuous(breaks=seq(0.08, 0.125, 0.005))
X = data.frame(x=rep(1:10,5),testerr=c(record[[1]]$testerr[1:10],record[[2]]$testerr,record[[3]]$testerr,
               record[[4]]$testerr,record[[5]]$testerr),label=c(rep('Rp=0.3,Rn=0.5',10),
               rep('Rp=0.2,Rn=0.5',10),rep('Rp=0.3,Rn=0.3',10),rep('Rp=0.3,Rn=0.2',10),rep('Rp=0.5,Rn=0.5',10)))
ggplot(X,aes(x,testerr,col=label)) +
  geom_line() + geom_point() + xlab('boost level') + ylab("validation test error")+
  scale_x_continuous(breaks=1:10) #+ scale_y_continuous(breaks=seq(0.08, 0.125, 0.005))
