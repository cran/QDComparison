\name{QDComparison-package}
\alias{QDComparison-package}
\alias{QDComparison}
\docType{package}
\title{Modern Nonparametric Tools for Two-Sample Quantile and Distribution Comparisons}
\description{Allows practitioners to determine (i) if two univariate distributions (which can be continuous, discrete, or even mixed) are equal, (ii) how two distributions differ (shape differences, e.g., location, scale, etc.), and (iii) where two distributions differ (at which quantiles), all using nonparametric LP statistics.}
%\details{
%\tabular{ll}{
%Package: QDComparison\cr
%Type: Package\cr
%Version: 2.0\cr
%Date: 2018-09-28\cr
%License: GPL 2\cr
%NeedsCompilation: no\cr
%Repository: CRAN\cr
%Date/Publication: 2017-11-20 09:20:35 UTC
%}
%
%}
\author{David Jungreis, Subhadeep Mukhopadhyay

Maintainer: David Jungreis <dbjungreis@gmail.com>

}
\references{ 
Jungreis, D. and Mukhopadhyay, S. (2018), "Two-Sample Quantile and Distribution Comparisons"

Mukhopadhyay, S. and Parzen, E. (2014), "LP Approach to Statistical Modeling", arXiv:1405.2601.
 }

\keyword{package}

\examples{
x <- c(rep(0,200),rep(1,200))
y <- c(rnorm(200,0,1),rnorm(200,1,1))
L <- LP.QDC(x,y)
L$pval
}