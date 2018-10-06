\name{NMES}
\alias{NMES}
\docType{data}
\title{
National Medicare Expenditure Survey (NMES) Data on Cost of Hospitalizations}
\description{
These data come from Venturini's (2015) study of hospital costs for patients with smoking and non-smoking diseases.}
\usage{data(NMES)}
\format{
  A data frame with 9416 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{A binary indicator variable: 0 for non-smoking disease, 1 for smoking disease}
    \item{\code{y}}{The response variable: cost of a hospital stay, in dollars}
  }
}
%\source{ 
%
%Dominici, F., Cope, L., Naiman, D. Q., and Zeger, S. L. (2005), "Smooth quantile ratio estimation," Biometrika, 92, 543-557.
%
%Dominici, F. and Zeger, S. L. (2005), "Smooth quantile ratio estimation with regression: estimating medical expenditures for smoking-attributable diseases," Biostatistics, 6, 505-519.
%
%Johnson, E., Dominici, F., Griswold, M., and Zeger, S. L. (2003), "Disease cases and their medical costs attributable to smoking: an analysis of the national medical expenditure survey," Journal of Econometrics, 112, 135-151.
%
%Venturini, S., Dominici, F., Parmigiani, G., et al. (2015), "Generalized quantile treatment effect: A flexible Bayesian approach using quantile ratio smoothing," Bayesian Analysis,10, 523-552.
%
%}
\references{ 

Dominici, F., Cope, L., Naiman, D. Q., and Zeger, S. L. (2005), "Smooth quantile ratio estimation," Biometrika, 92, 543-557.

Dominici, F. and Zeger, S. L. (2005), "Smooth quantile ratio estimation with regression: estimating medical expenditures for smoking-attributable diseases," Biostatistics, 6, 505-519.

Johnson, E., Dominici, F., Griswold, M., and Zeger, S. L. (2003), "Disease cases and their medical costs attributable to smoking: an analysis of the national medical expenditure survey," Journal of Econometrics, 112, 135-151.

Venturini, S., Dominici, F., Parmigiani, G., et al. (2015), "Generalized quantile treatment effect: A flexible Bayesian approach using quantile ratio smoothing," Bayesian Analysis, 10, 523-552.
}

\examples{
data(NMES)
## maybe str(NMES)
y <- NMES[,2]
x <- NMES[,1]
# Remove the 0s (as Venturini (2015) notes was necessary)
ind <- which(y==0)
x <- x[-ind]
y <- y[-ind]
hist(y[x==0])
}
\keyword{datasets}











