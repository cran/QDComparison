\name{Fundraising}
\alias{Fundraising}
\docType{data}
\title{
Gneezy's Fundraising Data with a Gift Wage}
\description{
These data come from Gneezy's (2006) fundraising experiment, on which Goldman (2018) performed quantile treatment effect analysis. These data correspond to the "pre-lunch" period.}
\usage{data(Fundraising)}
\format{
  A data frame with 23 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{A binary indicator variable: 0 for control, 1 for intervention (gift wage)}
    \item{\code{y}}{The response variable: dollars raised}
  }
}
\source{
Gneezy, U. and List, J. A. (2006), "Putting behavioral economics to work:  Testing for gift exchange in labor markets using field experiments," Econometrica, 74, 1365-1384.
}
\references{Gneezy, U. and List, J. A. (2006), "Putting behavioral economics to work:  Testing for gift exchange in labor markets using field experiments," Econometrica, 74, 1365-1384.

Goldman, M. and Kaplan, D. M. (2018), "Comparing distributions by multiple testing across quantiles or CDF values," Journal of Econometrics, Volume 206, Issue 1, 143-166.}
\examples{
data(Fundraising)
## maybe str(Fundraising)
y <- Fundraising[,2]
x <- Fundraising[,1]
hist(y[x==1])
}
\keyword{datasets}