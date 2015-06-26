\name{convertAverage}
\alias{convertAverage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Take averages over a list of data.frames
}
\description{
Given a list of similar data.frames (same schema and dimension) or vectors (same length), create a single data.frame or vector with same dimension or length consisting of averages over the list.
}
\usage{
convertAverage(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A list of equally-sized data.frames or vectors
}
}
\value{
A single data.frame or vector containing averages over the list.
}
\author{
Wannes Rosiers
}

\examples{
# Create a list of data.frames
frame1 <- data.frame(a=1:10,b=2:11)
frame2 <- data.frame(a=11:20,b=3:12)
frameList <- list(frame1,frame2)

# Take the respective averages
convertAverage(frameList)

# Create a list of vectors
vector1 <- 1:10
vector2 <- 11:20
vectorList <- list(vector1,vector2)

# Take the respective averages
convertAverage(vectorList)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
