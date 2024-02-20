fixregday1.pck <-
c("fixregday1.pck", "regday1.pck", "anscombe.fit1", "anscombe.fit2", 
"gauss.reg1", "NOAAGISSWD", "my.hat.w", "bootmatsmooth", "bootmatlin", 
"gen.inv1", "is.inf")
regday1.pck <-
c("regday1.pck", "anscombe.fit1", "anscombe.fit2", "gauss.reg1", 
"NOAAGISSWD", "my.hat.w", "bootmatsmooth", "bootmatlin", "gen.inv1", 
"is.inf")
anscombe.fit1 <-
function(){
for(i in 1:4){
 ls.print(lsfit(anscombe[[i]],anscombe[[i+4]]))
 }
}
anscombe.fit2 <-
function(){
par(mfrow=c(2,2))
for(i in 1:4){
 plot(anscombe[[i]],anscombe[[i+4]])
abline(lsfit(anscombe[[i]],anscombe[[i+4]]),col=2)
}
}
gauss.reg1 <-
function(x,y,lambda,xcol=4,do.plot=T)
{
o1<-order(x)
x1<-x[o1]
y1<-y[o1]
r1<-range(x)
smat<-NULL
n1<-length(x1)
for(i in 1:n1){
        v1<-dnorm(x1,x1[i],lambda)
        v1<-v1/sum(v1)
        H1<-my.hat.w(x1,v1)
        smat<-rbind(smat,H1[i,])
}
yhat<-smat%*%y1
if(do.plot){
lines(x1,yhat,col=xcol)
}
n99<-length(x1)
dferror<-length(x1)-sum(diag(2*smat-smat%*%(t(smat))))
delta1<-sum(diag(t(diag(n99)-smat)%*%(diag(n99)-smat)))
R<-t(diag(n99)-smat)%*%(diag(n99)-smat)
delta2<-2*sum(diag(R%*%R))
resid<-y1-smat%*%y1
ypred<-y1
ypred[o1]<-smat%*%y1
PRESS<-sum((resid/(1-diag(smat)))^2)
list(smat=smat,df=sum(diag(smat)),dferror=dferror,delta1=delta1,delta2=delta2,resid=resid,pred.out=ypred[o1],x.out=x1,press=PRESS,cor=cor(ypred,y))
}
NOAAGISSWD <-
structure(list(Year = 1980:2023, Drought.Count = c(1L, 0L, 0L, 
1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 
1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L), Flooding.Count = c(1L, 0L, 
0L, 3L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 2L, 2L, 
1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 2L, 2L, 0L, 2L, 
1L, 2L, 4L, 2L, 0L, 3L, 0L, 2L, 1L, 4L), Freeze.Count = c(0L, 
1L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L), Severe.Storm.Count = c(0L, 
1L, 2L, 0L, 2L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 4L, 1L, 1L, 2L, 0L, 
1L, 4L, 1L, 1L, 2L, 2L, 4L, 2L, 1L, 5L, 1L, 6L, 6L, 4L, 10L, 
7L, 7L, 7L, 6L, 8L, 11L, 10L, 8L, 13L, 11L, 11L, 19L), Tropical.Cyclone.Count = c(1L, 
0L, 0L, 1L, 0L, 3L, 0L, 0L, 0L, 2L, 0L, 1L, 2L, 0L, 1L, 3L, 1L, 
0L, 3L, 1L, 0L, 1L, 2L, 1L, 4L, 4L, 0L, 0L, 3L, 0L, 0L, 2L, 2L, 
0L, 0L, 0L, 1L, 3L, 2L, 2L, 7L, 4L, 3L, 2L), Wildfire.Count = c(0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 
0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 
0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), Winter.Storm.Count = c(0L, 
0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 2L, 0L, 1L, 
0L, 1L, 2L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 2L, 0L, 
0L, 1L, 1L, 0L, 0L, 2L, 0L, 0L, 1L, 1L, 1L), All.Disasters.Count = c(3L, 
2L, 3L, 6L, 2L, 7L, 3L, 0L, 1L, 6L, 4L, 4L, 7L, 5L, 6L, 7L, 5L, 
3L, 11L, 5L, 5L, 3L, 6L, 7L, 6L, 6L, 8L, 5L, 12L, 9L, 7L, 18L, 
11L, 10L, 10L, 11L, 15L, 19L, 16L, 14L, 22L, 20L, 18L, 28L), 
    delta.temp = c(0.27000000000000002, 0.33000000000000002, 
    0.13, 0.29999999999999999, 0.16, 0.12, 0.19, 0.33000000000000002, 
    0.40999999999999998, 0.28999999999999998, 0.44, 0.42999999999999999, 
    0.23000000000000001, 0.23999999999999999, 0.32000000000000001, 
    0.45000000000000001, 0.34999999999999998, 0.47999999999999998, 
    0.63, 0.41999999999999998, 0.41999999999999998, 0.54000000000000004, 
    0.63, 0.62, 0.55000000000000004, 0.68999999999999995, 0.64000000000000001, 
    0.66000000000000003, 0.54000000000000004, 0.65000000000000002, 
    0.72999999999999998, 0.60999999999999999, 0.64000000000000001, 
    0.66000000000000003, 0.75, 0.90000000000000002, 1.02, 0.93000000000000005, 
    0.84999999999999998, 0.98999999999999999, 1.02, 0.84999999999999998, 
    0.89000000000000001, 1.1699999999999999)), class = "data.frame", row.names = c(NA, 
-44L))
my.hat.w <-
function(x,wt){
x1<-cbind(1,x)
x1%*%gen.inv1(t(x1)%*%diag(wt)%*%x1)%*%t(x1)%*%(diag(wt))
}
bootmatsmooth <-
function(x,y,lambda=.06,rfunc=gauss.reg1,nboot=1000){
par(mfrow=c(2,2))
plot(x,y)
plot(x,y)
predmat<-NULL
m1<-cbind(y,x)
n1<-length(y)
corvec<-NULL
for(i in 1:nboot){
	b1<-sample(c(1:n1),replace=T)
	mboot<-m1[b1,]
	yboot<-mboot[,1]
	xboot<-mboot[,-1]
	dum<-rfunc(xboot,yboot,lambda,do.plot=F)
	lines(dum$x.out,dum$pred.out,col=2)
	corvec<-c(corvec,dum$cor)
}
dum1<-rfunc(x,y,lambda)
lines(dum1$x.out,dum1$pred.out)
plot(density(corvec))
lines(c(cor(x,y),cor(x,y)),c(0,12))
}
bootmatlin <-
function(x,y,lambda=.06,rfunc=lsfit,nboot=1000){
par(mfrow=c(2,2))
plot(x,y)
plot(x,y)
predmat<-NULL
m1<-cbind(y,x)
n1<-length(y)
corvec<-NULL
for(i in 1:nboot){
	b1<-sample(c(1:n1),replace=T)
	mboot<-m1[b1,]
	yboot<-mboot[,1]
	xboot<-mboot[,-1]
	dum<-rfunc(xboot,yboot)
	dum$x.out<-sort(xboot)
	dum$pred.out<-dum$coef[1]+dum$coef[2]*dum$x.out
	lines(dum$x.out,dum$pred.out,col=2)
	corvec<-c(corvec,cor(xboot,yboot))
}
dum1<-rfunc(x,y)
dum$x.out<-sort(x)
	dum$pred.out<-dum$coef[1]+dum$coef[2]*dum$x.out

lines(dum1$x.out,dum1$pred.out)
plot(density(corvec))
lines(c(cor(x,y),cor(x,y)),c(0,12))
}
gen.inv1 <-
function(mat, thresh = 1e-10)
{
	v1 <- sum(is.na(mat))
	v2 <- sum(is.inf(mat))
	if((v1 + v2) > 0.5) {
		print(mat)
	}
	e1 <- eigen(mat, symmetric = T)
	val <- Re(e1$val)
	vec <- Re(e1$vec)
	val1 <- val/max(val)
	#
	#	print("normalized eigen values")
	#	print(val1)	#
	#	n1 <- length(val1)
	#	plot(c(1:n1), abs(val1), log = "y", xlab = "eigen rank", ylab
	#		 = "log10 of value")
	I1 <- val1 > thresh
	I3 <- is.na(I1)
	if(sum(I3) < 0.5) {
		val2 <- val[I1]
		I2 <- I1
		if(sum(I2) > 1.5) {
			ret <- vec[, I1] %*% diag(1/val2) %*% t(vec[, I1])
		}
		else {
			v1 <- as.matrix(vec[, I1], length(c(vec[, I1])), 1)
			ret <- (1/val2) * v1 %*% t(v1)
		}
	}
	else {
		ret <- diag(length(I1)) * 0
	}
	ret
}
is.inf <-
function(z){z==Inf}
