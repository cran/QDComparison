QDC.Permute <-
function(x,y,B=500,m=6,alpha=.05){
mat = matrix(NA, nrow = B, ncol = length(y)) #yes, it's empty for now
u_obs <-  ecdf(y)(y)
for(i in 1:B){
x_new <- sample(x)
lp.boot.coef <- LP.XY(x_new,y,m,smooth="FALSE")$LP.coef.1
S_new <- eLP.poly(y,m)
d.hat <- 1 + S_new%*%lp.boot.coef
d.hat[d.hat<0]<-0
u_new <- ecdf(y)(y)
dfun <- approxfun(u_new, d.hat, rule=2)
mat[i,] <- dfun(u_obs)
}

Q <- matrix(, 2, ncol(mat))

for(j in 1:ncol(mat)){
Q[1,j] <- quantile(mat[,j],alpha/2)
Q[2,j] <- quantile(mat[,j],1 - alpha/2)
}
return(Q)
}
