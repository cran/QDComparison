\name{Microfinance}
\alias{Microfinance}
\docType{data}
\title{
Informal Borrowing in Neighborhoods of Hyderabad, India}
\description{
These data come from Banerjee's (2015) informal borrowing observations.}
\usage{data(Microfinance)}
\format{
  A data frame with 6811 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{A binary indicator variable: 0 for control, 1 for intervention (access to microfinance)}
    \item{\code{y}}{The response variable: rupees informally borrowed}
  }
}
\source{https://www.aeaweb.org/articles?id=10.1257/app.20130533 }
\references{Banerjee, A., Duflo, E., Glennerster, R., and Kinnan, C. (2015), "The miracle of microfinance? Evidence from a randomized evaluation," American Economic Journal: Applied Economics, 7, 22-53. }
\examples{
data(Microfinance)
## maybe str(Microfinance)
y <- Microfinance[,2]
x <- Microfinance[,1]
# Remove the 0s (as Banerjee (2015) appears to have done)
ind <- which(y==0)
x <- x[-ind]
y <- y[-ind]
hist(y[x==0])
}
\keyword{datasets}











