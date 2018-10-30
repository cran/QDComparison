\name{LP.QDC}
\alias{LP.QDC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The main function for two-sample quantile and distribution comparison

}
\description{
This function runs the entire quantile and distribution comparison, giving LP comeans, LP Fourier coefficients, LPINFOR test statistic, p-value, estimated comparison density with null-band, and intervals where the comparison density is above or below the null band

}
\usage{
LP.QDC(x,y,m=6,smooth="TRUE",method="BIC",alpha=0.05,
       B=1000,spar=NA,plot="TRUE",inset=-0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Indicator variable denoting group membership
}
  \item{y}{
Response variable with measured values
}

  \item{m}{
Number of LP comeans and LP Fourier coefficients to be calculated, default: 6
}

  \item{smooth}{
If smoothing should be applied, default: TRUE
}

  \item{method}{
Smoothing method, default: BIC
}

  \item{alpha}{
Alpha-level for confidence bands, default: 0.05
}

  \item{B}{
Number of permutations of the x labels, default: 1000
}



  \item{spar}{
"spar" in "smooth.spline" of upper and lower bounds of confidence bands, default: NA, let smooth.splines function figure it out
}

  \item{plot}{
Should plotting be performed, default: TRUE
}

  \item{inset}{
Graphical parameter that controls where the color legend is plotted below x-axis, default: -0.2
}


}

\author{
David Jungreis

Subhadeep Mukhopadhyay
}

\references{ 
Jungreis, D. and Mukhopadhyay, S. (2018), "Two-Sample Quantile and Distribution Comparisons"

 }

\value{

A list containing:

  \item{band}{
y-values of the upper and lower bounds of the confidence band
}

  \item{d.hat}{
y-values of the comparison density
}

  \item{sparL}{
"spar" value in "smooth.spline" of lower bound of the null band
}
  \item{sparU}{
"spar" value in "smooth.spline" of upper bound of the null band
}

  \item{out.above}{
Quantile intervals where f1 dominates the pooled distribution
}

  \item{out.below}{
Quantile intervals where f0 dominates the pooled distribution
}

  \item{LP.comeans.0 }{
LP comeans, conditioned on X=0
}

  \item{LP.coef.0}{
LP Fourier coefficients, conditioned on X=0
}

  \item{LP.comeans.1}{
LP comeans, conditioned on X=1
}

  \item{LP.coef.1}{
LP Fourier coefficients, conditioned on X=1
}

  \item{LPINFOR}{
Test statistics value
}

  \item{pval}{
The p-value for testing equality of two distributions F0=F1
}
}

\examples{
x <- c(rep(0,200),rep(1,200))
y <- c(rnorm(200,0,1),rnorm(200,1,1))
L <- LP.QDC(x,y)
L$pval
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Main Function}% use one of RShowDoc("KEYWORDS")
