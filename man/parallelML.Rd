\name{parallelML}
\alias{parallelML}
\alias{parallelML.default}
\alias{summary.parallelML}
\alias{print.parallelML}
\encoding{UTF-8}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parallel-Voting version of machine learning algorithms
}
\description{
By sampling your data, running the machine learning algorithm on these samples in parallel on your own machine and letting your models vote on a prediction, we return much faster predictions than the regular machine learning algorithm and possibly even more accurate predictions.
}
\usage{
\method{parallelML}{default}(MLCall, MLPackage,
			samplingSize = 0.2,numberCores = detectCores(),
			underSample = FALSE, underSampleTarget = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MLCall}{
Your call to a machine learning algorithm.  All arguments in this call should be named and the package only allows formula calls.  Hence the call should look like "machineLearningAlgorithm(formula = ..., data = ..., ...)".
}
  \item{MLPackage}{
A character string of the package which provides your machine learning algorithm.  This is needed since all cores should load the package.
}
  \item{samplingSize}{
Size of your data you will take in each sample.
}
  \item{numberCores}{
Number of cores of your machine you want to use.  Is set equal to the number of samples you take.
}
  \item{underSample}{
Logical wether you want to take an undersample on your desired target.	
}
  \item{underSampleTarget}{
When you set underSample to TRUE, underSampleTarget takes your target you want to keep in every sample.  e.g.  If you have 5 elements of category1 and 100 elements of category2 and your sampleSize is 0.2, then every sample will contain 25 elements, namely the 5 of category1 and 20 of category2.
}
}
\value{
A list containing of numberCores machine learning models.
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
\dontrun{
# Load the library which provides svm
library(e1071)

# Create your data
data(iris)
subdata <- iris[1:60,]

# Create a model
parsvmmodel   <- parallelML("svm(formula = Species ~ ., data = subdata)",
                            "e1071",samplingSize = 0.8,
                            underSample = TRUE, underSampleTarget = "versicolor")
parsvmpred    <- predictML("predict(parsvmmodel,newdata=subdata)",
                           "e1071","vote")
table(parsvmpred,subdata$Species)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.