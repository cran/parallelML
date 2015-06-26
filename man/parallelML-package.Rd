\name{parallelML-package}
\alias{parallelML-package}
\docType{package}
\title{
Parallel-Voting version of machine learning algorithms
}
\description{
By sampling your data, running the machine learning algorithm on these samples in parallel on your own machine and letting your models vote on a prediction, we return much faster predictions than the regular machine learning algorithm and possibly even more accurate predictions.
}
\details{
\tabular{ll}{
Package: \tab parallelML\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-09-06\cr
License: \tab GPL-2\cr
}
This package consists of two main functions:
\code{\link{parallelML}} A function which allows you to create multiple machine learning models: one for each core you provide.  It returns a list of machine learning models.
\code{\link{predictML}}: A function which creates predictions for all models you created.  When combine is "vote", it returns the class most models agree upon, otherwise it gives a list of predictions of all models
}
\author{
Wannes Rosiers

Maintainer: Wannes Rosiers <wannes.rosiers@infofarm.be>
}
\keyword{ package }
\seealso{
This package can be regarded as a parallel extension of machine learning algorithms, therefor check the package of the machine learning algorithm you want to use.
}
\examples{
\dontrun{
# Load the library which provides svm
library(e1071)

# Create your data
data(iris)

# Create a model
parSvmModel <- parallelML("svm(formula = Species ~ ., data = iris)",
                     "e1071",samplingSize = 0.8)

# Get prediction
parSvmPred   <- predictML("predict(parSvmModel,newdata=iris)",
                          "e1071","vote")

# Check the quality
table(parSvmPred,iris$Species)
}
}