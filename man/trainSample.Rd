\name{trainSample}
\alias{trainSample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample data in parallel
}
\description{
Sample data or data and output in parallel: each core provides one sample of your desired size.
}
\usage{
trainSample(data, numberCores = detectCores(), samplingSize = 0.2,
			underSample = FALSE, toPredict = NULL, underSampleTarget = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame, or structure convertable to a data frame, which you want to sample upon.
}
  \item{numberCores}{
In this setting equal to number of different training samples you are creating: one for each core you are using.
}
  \item{samplingSize}{
Size of your training sample in percentage.
}
  \item{underSample}{
Logical wether you want to take an undersample on your desired target.	
}
  \item{toPredict}{
The column of your dataset you want to predict  	
}
  \item{underSampleTarget}{
When you set underSample to TRUE, underSampleTarget takes your target you want to keep in every sample.  e.g.  If you have 5 elements of category1 and 100 elements of category2 and your sampleSize is 0.2, then every sample will contain 25 elements, namely the 5 of category1 and 20 of category2.
}
}
\value{
You get a list of length numberCores.  Each core has created one item of your list, namely a data frame containing a a samplingSize size sample of data.
}
\author{
Wannes Rosiers
}

\seealso{
Under the hood this function uses \code{\link[foreach]{foreach}}, and \code{\link[base]{sample}}
}

\examples{
\dontrun{
# Create your data
x <- data.frame(1:10,10:1)

# Sampling
trainSample(x,numberCores=2,samplingSize = 0.5)

# Create your data
data(iris)
data <- iris[1:110,]

# Sampling
trainSamples <- trainSample(data,2,0.2,TRUE,"Species","virginica")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
