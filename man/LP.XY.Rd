\name{LP.XY}
\alias{LP.XY}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute LP comoments, LP coefficients, LPINFOR test statistic, and a p-value of distribution equality
}
\description{
This function computes LP comoments, LP coefficients, LPINFOR test statistic, and the corresponding p-value of for testing equality of two distributions.
}
\usage{
LP.XY(x,y,m=6,smooth="TRUE",method="BIC")
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
Number of LP comoments and LP coefficients to be calculated, default: 6
}

  \item{smooth}{
If smoothing should be applied, default: TRUE
}

  \item{method}{
Smoothing method, default: BIC
}

}

\value{

A list containing:

  \item{LP.comoment.0 }{
LP comoments, conditioned on X=0
}

  \item{LP.coef.0}{
LP coefficients, conditioned on X=0
}

  \item{LP.comoment.1}{
LP comoments, conditioned on X=1
}

  \item{LP.coef.1}{
LP coefficients, conditioned on X=1
}

  \item{LPINFOR}{
Test statistics value
}

  \item{pval}{
The p-value for testing equality of two distributions F0=F1
}

}


\author{
Subhadeep Mukhopadhyay

David Jungreis

}

\references{ 
Jungreis, D., (2019) "Unification of Continuous, Discrete, and Mixed Distribution Two-Sample Testing with Inferences in the Quantile Domain"

Mukhopadhyay, S. and Parzen, E. (2014), "LP Approach to Statistical Modeling", arXiv:1405.2601.
 }
 
 
\examples{
x <- c(rep(0,200),rep(1,200))
y <- c(rnorm(200,0,1),rnorm(200,1,1))
L <- LP.XY(x,y)
L$pval
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Helper Functions}% use one of RShowDoc("KEYWORDS")