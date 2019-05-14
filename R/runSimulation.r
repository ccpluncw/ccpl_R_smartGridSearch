#' Function to run the simulation for the smartGridSearch.
#'
#' This function runs the simulation within the smartGridSearch optimization program.  It runs "fn" once and outputs the parameter values and the fit statistic.
#'
#' @param fn A function that runs the model you would like to optimize.  It should return the fit statistic that is to be minimized.
#' @param parsUpper A list containing, for each parameter, a vector of values for the upper bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."
#' @param parsLower A list containing, for each parameter, a vector of values for the lower bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."
#' @param parsMinInt A list containing, for each parameter, a vector of values for the minimum interval sizes to be used when randomly choosing a value between the upper and lower bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."
#' @param otherParamList A list containing the other arguments/parameters to be passed to "fn."  The name of each element in the list must be the arguments/parameters name in "fn."
#' @param numIntervals An integer that specifies the number of intervals that the range between the upper and lower bounds of each parameter space will be segmented into.   DEFAULT = 25.
#'
#' @return A list containing the parameter values for this run of the simulation and the fit statistic that is to be minimized
#'
#' @keywords simulation Grid Search Smart
#' @export
#' @examples runSimulation (fn, optParsUpper = list(a = (a1 = 1, a2 = 1)), optParsLower = list(a = (a1 = 0, a2 = 0)), parsMinInt = list(a = (a1 = 0.01, a2 = 0.01)), otherParamList = list( data = data.frame ( ... ), ...), numIntervals = 25)


runSimulation <- function (fn, optParsUpper, optParsLower, parsMinInt, otherParamList, numIntervals) {
  parNames <- names(parsMinInt)
  parList <- list()

  #fill each parameter in the list with random values within the parameter space
  for(i in 1:length(parNames)) {
    tmp.up <- optParsUpper[[parNames[i]]]
    tmp.lo <- optParsLower[[parNames[i]]]
    tmp.int <- parsMinInt[[parNames[i]]]
    tmp.pars <- NULL
    for(j in 1:length(tmp.up)) {
      #calculate interval value
      intVal <- (tmp.up[j] - tmp.lo[j])/numIntervals
      #ensure interval value is not less than the minimum interval value
      intVal <- ifelse(intVal < tmp.int[j], tmp.int[j], intVal)
      #get the randomly chosen parameter value
      tmp.pars[j] <- as.numeric(getRandomNumber( 1, tmp.lo[j], tmp.up[j], intVal))
    }
    parList[[parNames[i]]] <- tmp.pars
  }

  #add the other parameters for the simulation
  if(!is.null(otherParamList)) {
    parList <- c(parList, otherParamList)
  }

  #run the simulation with the parameters filled above and get the fit statistic that will be minimized
  r_out <- NA
  tryCatch ({
      r_out <- do.call(fn,parList)
  }, error = function(e) {})

  #remove the other parameters from the parameter list
  if(!is.null(otherParamList)) {
    otherParamNames <- names(otherParamList)
    for(i in otherParamNames) {
      parList[[i]] <- NULL
    }
  }

  #convert the parameter list to a dataframe
  df.pars.tmp <- parList
  #add the fit statistic from the simulation and set the SD of that statistic to NA
  df.pars.tmp$r_out <- r_out
  df.pars.tmp$sdR_out <- NA

  #return pars
  return(list(df.pars.tmp))
}
