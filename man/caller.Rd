\name{caller}
\alias{caller}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert arguments of a function to a list
}
\description{
It strips the function call of a function and returns a named list of only the used arguments
}
\usage{
caller(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
All arguments you would pass to another function.
}
}
\value{
A named list of all arguments
}
\author{
Wannes Rosiers
}
\examples{
caller(formula = Species ~ ., data = iris)
}