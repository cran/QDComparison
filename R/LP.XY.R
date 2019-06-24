LP.XY <-
function(x,y,m=6,smooth="TRUE",method="BIC"){ 
    #- for two-sample A/B problem, x is binary group variable
    p1 <- length(x[x==1])/length(x)
    LP.com <- cov(eLP.poly(y,m), eLP.poly(x,m))
    df<- length(c(LP.com))
    if(smooth=="TRUE"){
        LP.com <- LP.smooth(LP.com,length(y),method) 
        df <- length(LP.com[abs(LP.com)>0])
    }
    LP.coef <- sqrt( (1-p1)/p1 )*LP.com
    lpinfor <- length(y)*sum(LP.com^2)
    pval <- 1- pchisq(lpinfor,df=df)
    
    # now define the LP comoments and LP coefficients for X=0
    # (Earlier steps do X=1)
    #
    LP.com.0 <- - LP.com
    LP.coef.0 <- (p1/(p1-1))*LP.coef
    
    
    L <- list()
    L$LP.comoment.0 <- as.vector(LP.com.0)
    L$LP.coef.0 <- as.vector(LP.coef.0)
    L$LP.comoment.1 <- as.vector(LP.com)
    L$LP.coef.1 <- as.vector(LP.coef)
    L$LPINFOR <- lpinfor
    L$pval <- pval
    
    return(L)
}
