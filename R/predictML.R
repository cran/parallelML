#' @importFrom parallel detectCores
#' @importFrom foreach foreach %do%
#' @export
predictML <- function(predictCall, MLPackage, combine = "raw"){
  if (!(combine %in% c("raw","vote"))){
    stop("combine should be raw or vote")
  } else {
    # Get the function call and the object
    fcall  <- getCall(predictCall)
    object <- eval(fcall$object)
    
    # Make the data available to all cores
    fcall$newdata <- eval(fcall$newdata)
    
    # Use the amount of cores the model was build with
    registerCores(length(object))
    
    # Create copies with the correct data
    function_call <- list()
    for (i in 1:length(object)){
      function_call[[i]]              <- fcall
      function_call[[i]]$object       <- object[[i]]
    }
    
    # parallel prediction
    predictData <- foreach(i = 1:length(object))  %do% {
      # Make the package available to every core
      library(MLPackage,character.only=TRUE)
      # Do the call
      eval(function_call[[i]], parent.frame())
    }
    
    # combine the predictions: raw returns just a list of prediction
    if (combine == "raw"){
      return(predictData)
    } else if (combine == "vote"){
      # the factors
      result <- as.factor(apply(data.frame(predictData),
                                1,function(vec){names(sort(table(vec),decreasing=TRUE))[1]}))
      return(result)
    } else{
      return("to do")
    }
  }
}