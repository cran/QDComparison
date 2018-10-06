\name{eLP.poly}
\alias{eLP.poly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to compute the LP basis functions
}
\description{
Given a random sample from X (which can be discrete, continuous, or even mixed), this function computes the empirical LP-basis functions.
}
\usage{
eLP.poly(x,m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The random samples
}
  \item{m}{
Number of basis functions to compute
}

}

\value{

  LP basis functions  
 
}

\author{
Subhadeep Mukhopadhyay
}

\references{ 
Mukhopadhyay, S. and Parzen, E. (2014), "LP Approach to Statistical Modeling", arXiv:1405.2601.

Mukhopadhyay, S., and Wang, K (2018), "A Nonparametric Approach to High-dimensional K-sample Comparison Problem".  
 }
\examples{
x <- c(rep(0,200),rep(1,200))
m <- 6
eLP.poly(x,m)}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Helper Functions}% use one of RShowDoc("KEYWORDS")