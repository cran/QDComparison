\name{Earnings1978}
\alias{Earnings1978}
\docType{data}
\title{LaLonde's 1978 Earnings Data}
\description{
These data come from LaLonde's (1986) National Supported Work Demonstration (NSW) Data (Dehejia-Wahha Sample (1999)), used by Firpo (2007).
}
\usage{data(Earnings1978)}
\format{
  A data frame with 445 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{A binary indicator variable: 0 for control, 1 for intervention (received job training)}
    \item{\code{y}}{The response variable: earnings in 1978}
  }
}
\source{http://users.nber.org/~rdehejia/data/nswdata2.html }
\references{Dehejia, R. H. and Wahba, S. (1999), "Causal effects in nonexperimental studies: Reevaluating the evaluation of training programs," Journal of the American Statistical Association, 94, 1053-1062.

Firpo, S. (2007), "Efficient semiparametric estimation of quantile treatment effects," Econometrica, 75, 259-276.

LaLonde, R. J. (1986), "Evaluating the econometric evaluations of training programs with experimental data," The American Economic Review, 604-620.}
\examples{
data(Earnings1978)
## maybe str(Earnings1978)
y <- Earnings1978[,2]
x <- Earnings1978[,1]
hist(y[x==1])
}
\keyword{datasets}
