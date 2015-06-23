\name{getCall}
\alias{getCall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert string to function call
}
\description{
Convert string to function call, where you can acces easily all entries
}
\usage{
getCall(MLCall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MLCall}{
Your call to a machine learning algorithm.  All arguments in this call should be named.
}
}
\value{
A function Call of class "call"
}
\author{
Wannes Rosiers
}
\examples{
\dontrun{
library(e1071)
MLCall <- "svm(formula = Species ~ ., data = iris)"
functionCall <- getCall(MLCall)
functionCall$formula
}
}