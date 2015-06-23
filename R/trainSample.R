#' @export
trainSample <- function(data, numberCores = detectCores(), samplingSize = 0.2,
                        underSample = FALSE, toPredict = NULL, underSampleTarget = NULL){
  # Function to create a sample of x and y
  
  # Use the amount of cores provided
  registerCores(numberCores)
  
  # Sample the data
  if (underSample == FALSE){
    trainSamples <- foreach(i = 1:numberCores) %dopar% {
      data[sample(1:nrow(data),
                  size=samplingSize*nrow(data), replace=TRUE),]
    }
  } else {
    if (is.null(underSampleTarget)){
      stop("No underSampleTarget provided")
    } else if (gregexpr(pattern =' ',toPredict)[[1]][1] != -1){
      stop("Can not undersample when two columns are predicted")
    } else {
      targets    <- which(data[,toPredict]==underSampleTarget)
      noTargets  <- setdiff(1:dim(data)[1],targets)
      targetData <- data[targets,]
      sampleData <- data[noTargets,]
      
      trainSamples <- foreach(i = 1:numberCores) %dopar% {
        rbind(sampleData[sample(1:nrow(sampleData),
                                size=samplingSize*nrow(sampleData), replace=TRUE),],
              targetData)
      }
    }
  }
    
  return(trainSamples)

} 