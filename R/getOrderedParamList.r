#' Function to identify and return the top N runs of the simulation (sorted)
#'
#' This function identifies the best N (optParamListN) runs of the simulation and returns them sorted by best run to worst run.
#'
#' @param paramList A list of lists, with each sublist containing the parameter values for a run of the simulation.  The top level list contains all the runs of the simulation.
#' @param listItemToMinimize An string specifying the name of the list item that specifies the fit statistic being minimized.  This list item tells the program how well the simulation parameters fit the data. DEFAULT = "r_out" (which is what smartGridSearch uses)
#' @param optParamListN An integer that specifies the number of best fit runs that will be used to extract the new bounds for next set of grid search loops.  So, if optParamListN == 10, then the parameter values from the 10 best runs will be used to refine the upper and lower bounds of for each parameter. DEFAULT = 10.
#' @param fn A function that runs the model you would like to optimize.  It should return the fit statistic that is to be minimized.
#' @param otherParamList A list containing the other arguments/parameters to be passed to "fn."  The name of each element in the list must be the arguments/parameters name in "fn."
#' @param optBoundLoops An integer that specifies the number of times to re-simulate the best fit run in order to get a mean and SD for the minimized fit statistic. If the mean fit statistic is no longer the best fit, the program will move to the next best fit run, and so on until the it identifies the best mean fit run. DEFAULT = 10.
#'
#' @return A list of lists containing a sorted list of the 10 best runs with the sublists containing the parameter values of each run.
#'
#' @keywords optimize ordered fit statistic list parameters
#' @export
#' @examples getOrderedParamList(parameterListOfLists, "r_out", optParamListN = 10, fn, otherParamList, optBoundLoops = 10)


getOrderedParamList <- function(paramList, listItemToMinimize = "r_out", optParamListN = 10, fn, otherParamList = NULL, optBoundLoops=10) {

    #order the parameter table and get top 10
    orderedParams <- paramList[order(sapply(paramList,'[[',listItemToMinimize))]
    orderedParams <- orderedParams[1:optParamListN]

    #check to see if the best run is already validated.  If so, set done to true and move on.  If not, validate it
    done <- ifelse(is.na(orderedParams[[1]]$sdR_out), FALSE, TRUE)

    #validate the runs runs by running them several times and getting a mean r_out and see if it is still the best
    while(done == FALSE) {
      #get the parameters to send to the simulation
      tmpPars <- orderedParams[[1]]
      tmpPars$r_out <- NULL
      tmpPars$sdR_out <- NULL

      if(!is.null(otherParamList)) {
        tmpPars <- c(tmpPars, otherParamList)
      }

      r_out <- NULL
      for(i in 1:optBoundLoops) {
        #simulate
        r_out[i] <- do.call(fn,tmpPars)
      }

      #calculate average results of simulation
      mR_out <- mean(r_out, na.rm=T)
      sdR_out <- sd(r_out, na.rm=T)
      orderedParams[[1]]$r_out <- mR_out
      orderedParams[[1]]$sdR_out <- sdR_out

      #re-order
      orderedParams <- orderedParams[order(sapply(orderedParams,'[[',listItemToMinimize))]

      #see if the best run has been validated
      done <- ifelse(is.na(orderedParams[[1]]$sdR_out), FALSE, TRUE)
    }

    return(orderedParams)

}
