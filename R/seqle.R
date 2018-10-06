seqle <-
function(x,incr=1) {

# From Carl Witthoft of Stack Overflow
# https://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers

  if(!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + incr
  i <- c(which(y|is.na(y)),n)
  list(lengths = diff(c(0L,i)),
       values = x[head(c(0L,i)+1L,-1L)])
}
