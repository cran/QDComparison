\name{Depression}
\alias{Depression}
\docType{data}
\title{
Jackson's CESD Depression Scores
}
\description{
The data come from Jackson's (2009) depression data, used by Wilcox (2014).
}
\usage{data(Depression)}
\format{
  A data frame with 372 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{A binary indicator variable: 0 for control, 1 for intervention (received therapy)}
    \item{\code{y}}{The response variable: CESD score (higher means more depressed)}
  }
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{Jackson, J., Mandel, D., Blanchard, J., Carlson, M., Cherry, B., Azen, S., Chou, C.P.,Jordan-Marsh, M., Forman, T., White, B., et al. (2009), "Confronting challenges in intervention research with ethnically diverse older adults: the USC Well Elderly II trial," Clinical Trials, 6, 90-101.

Wilcox, R. R., Erceg-Hurn, D. M., Clark, F., and Carlson, M. (2014), "Comparing two independent groups via the lower and upper quantiles," Journal of Statistical Computation and Simulation, 84, 1543-1551.
}
\examples{
data(Depression)
## maybe str(Depression)
y <- Depression[,2]
x <- Depression[,1]
hist(y[x==1])
}
\keyword{datasets}
