\name{getArgs}
\alias{getArgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get all arguments of a machine learning call
}
\description{
Converts a machine learning call to a named list of all its arguments.
}
\usage{
getArgs(MLCall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MLCall}{
Your call to a machine learning algorithm.  All arguments in this call should be named.
}
}
\value{
A named list of all arguments present in your machine learning call
}
\author{
Wannes Rosiers
}
\examples{
\dontrun{
library(e1071)
MLCall <- "svm(formula = Species ~ ., data = iris)"
getArgs(MLCall)
}
}