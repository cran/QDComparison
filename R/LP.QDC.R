LP.QDC <-
function(x,y,m=6,smooth="TRUE",method="BIC",alpha=0.05,B=1000,spar=NA,plot="TRUE",inset=-0.2){

# inset is a graphical parameter that controls where the color legend is plotted

L <- LP.XY(x,y,m,smooth,method)

if(sum(L$LP.comean.1)^2 != 0){
m <- max(max(which(L$LP.comean.1!=0)),1)

L <- LP.XY(x,y,m,smooth,method)

y1<-sort(y)
S <- eLP.poly(y1,m)
d.hat <- 1 + S%*%L$LP.coef.1
d.hat[d.hat<0]<-0
u <- ecdf(y1)(y1)
Q <- QDC.Permute(x,y,B=B,m=m,alpha=alpha)
out <- QTE.Out(d.hat,u,Q,spar) # Contains function from Carl Witthoft of Stack Overflow
# https://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers
#
if (is.na(spar)){
sparL <- out$sparL
sparU <- out$sparU
} else {
sparL <- sparU <- spar
}

if(plot=="TRUE"){
plot(c(0,u),c(d.hat,tail(d.hat,1)),col="darkred", lwd=2,type="s",ylim=c(0.95*min(d.hat),1.05*max(d.hat)),
main="Comparison Density",xlab="",ylab="")
lines(smooth.spline(c(0,u),c(Q[1,],tail(Q[1,],1)), spar=sparL),col="blue",lty=2)
lines(smooth.spline(c(0,u),c(Q[2,],tail(Q[2,],1)), spar=sparU),col="blue",lty=2)

for (i in 1:length(out$mat_above)){
axis(side=1,at=out$mat_above[i],col="mediumvioletred",lwd=4,labels="")
}

for (i in 1:nrow(out$mat_above)){

tics <- seq(out$mat_above[i,1],out$mat_above[i,2],((out$mat_above[i,2]-out$mat_above[i,1])/20))

for (j in 1:length(tics)){

axis(side=1,at=tics[j],col="mediumvioletred",lwd=1,labels="")

}
}

for (i in 1:length(out$mat_below)){
axis(side=1,at=out$mat_below[i],col="green4",lwd=4,labels="")
}

for (i in 1:nrow(out$mat_below)){

tics <- seq(out$mat_below[i,1],out$mat_below[i,2],((out$mat_below[i,2]-out$mat_below[i,1])/20))

for (j in 1:length(tics)){

axis(side=1,at=tics[j],col="green4",lwd=1,labels="")

}
}

title(ylab=expression(hat(d)), line=2)

legend("bottom",legend=c("Pooled distribution domination by f0     ",
"Pooled distribution domination by f1"),col=c("green4","mediumvioletred"),
bty="n",ncol=2,cex=0.7,pt.cex=0.7,pch=rep(c(16,18),each=4),xpd=TRUE,inset=inset)
}

output <- list()
output$band <- Q
output$d.hat <-d.hat
output$sparL <- sparL
output$sparU <- sparU
output$out.above <- out$mat_above
output$out.below <- out$mat_below
output$LP.comeans.0 <- L$LP.comean.0
output$LP.coef.0 <- L$LP.coef.0
output$LP.comeans.1 <- L$LP.comean.1
output$LP.coef.1 <- L$LP.coef.1
output$LPINFOR <- L$LPINFOR
output$pval <- L$pval
return(output)
}
else{print("All LP Fourier Coefficients are zero!")
print("Perhaps try a less aggressive smoothing method.")}
}
