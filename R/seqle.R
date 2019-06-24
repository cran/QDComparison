seqle <-
function (x, incr = 1) 
{
    if (!is.numeric(x)) 
        x <- as.numeric(x)
    n <- length(x)
    y <- x[-1L] != x[-n] + incr
    i <- c(which(y | is.na(y)), n)
    list(lengths = diff(c(0L, i)), values = x[head(c(0L, i) + 
                                                       1L, -1L)])
}
