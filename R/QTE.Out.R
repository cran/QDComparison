QTE.Out <-
function(d.hat,u,Q,spar=0.45){

OUT <- list()
dhat <- c(d.hat,tail(d.hat,1))
Qs <- c(0,u)
if (is.na(spar)){
lower <- smooth.spline(Qs,c(Q[1,],tail(Q[1,],1)), cv=F)
upper <- smooth.spline(Qs,c(Q[2,],tail(Q[2,],1)), cv=F)
sparL <- lower$spar
sparU <- upper$spar
} else {
lower <- smooth.spline(Qs,c(Q[1,],tail(Q[1,],1)), spar=spar)
upper <- smooth.spline(Qs,c(Q[2,],tail(Q[2,],1)), spar=spar)
sparL <- sparU <- spar
}

above <- below <- c()

for (i in 1:length(dhat)){

Vi <- dhat[i]
Qi <- Qs[i]
Li <- predict(lower,Qi)$y
Ui <- predict(upper,Qi)$y
if (Vi<Li){
below <- c(below,i)
}
if (Vi>Ui){
above <- c(above,i)
}
}

below_jumps <- below[which(diff(below) != 1)]
above_jumps <- above[which(diff(above) != 1)]

below_endpoints <- c(below_jumps,below[(below_jumps+1)])
above_endpoints <- c(above_jumps,above[(above_jumps+1)])

below_endpoints <- sort(c(below_jumps,min(below),max(below)))
above_endpoints <- sort(c(above_jumps,min(above),max(above)))

# Function from Carl Witthoft of Stack Overflow
# https://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers
#
seqle_below <- seqle(below)
seqle_above <- seqle(above)

below_ints <- c()
above_ints <- c()

for (i in 1:length(seqle_below$values)){

below_ints <- c(below_ints,seqle_below$values[i],(seqle_below$values[i]+seqle_below$lengths[i]-1))

} 

for (i in 1:length(seqle_above$values)){

above_ints <- c(above_ints,seqle_above$values[i],(seqle_above$values[i]+seqle_above$lengths[i]-1))

} 

below_ints <- sort(below_ints)
above_ints <- sort(above_ints)

mat_below <- t(matrix(Qs[below_ints],2))
mat_above <- t(matrix(Qs[above_ints],2))

OUT$below_ints <- below_ints
OUT$above_ints <- above_ints
OUT$mat_below <- mat_below
OUT$mat_above <- mat_above
OUT$sparU <- sparU
OUT$sparL <- sparL
return(OUT)
}
