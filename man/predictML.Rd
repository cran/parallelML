\name{predictML}
\alias{predictML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel-Voting prediction of multiple machine learning models
}
\description{
By sampling your data, running the machine learning algorithm on these samples in parallel on your own machine and letting your models vote on a prediction, we return much faster predictions than the regular machine learning algorithm and possibly even more accurate predictions.
}
\usage{
predictML(predictCall, MLPackage, combine = "raw")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predictCall}{
Your call to a predict function.  The object however should be an element of class parallelML.
}
  \item{MLPackage}{
A character string of the package which provides your machine learning algorithm.  This is needed since all cores should load the package.
}
  \item{combine}{
A character string: "raw" when you want to return a list of predictions, "vote" when you want to return the class upon which most models agree.
}
}
\value{
Either a list of vectors of predictions for each model or a vector of class upon which most models agree.
}
\author{
Wannes Rosiers
}
\note{
Although it can cope with numeric probability predictions, this package is designed for classification labeling.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

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
\dontrun{
# Load the library which provides rpart
library(rpart)

# Create your data
data("magicData")

# Create a model
parTreeModel  <- parallelML("rpart(formula = V11 ~ ., data = trainData[,-1])",
                            "rpart",samplingSize = 0.8)

# Get prediction
parTrainTreePred  <- predictML("predict(parTreeModel,newdata=trainData[,-1],type='class')",
                               "rpart","vote")
parTestTreePred  <- predictML("predict(parTreeModel,newdata=testData[,-1],type='class')",
                              "rpart","vote")

# Check the quality
table(parTrainTreePred,trainData$V11)
table(parTestTreePred,testData$V11)	
}
}