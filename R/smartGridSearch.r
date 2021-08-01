#' Grid Search Optimization Function
#'
#' This is a "smart" grid search optimization function. For each parameter, it randomly selects valid values within a set of bounds.  It then runs the model to be optimized and saves the fit statistic.  The smart grid search optimization repeats this numLoops number of times (Default = 200).  The smart grid search then reduces the bounds of each parameter based on the spread of the parameters in the top N fits (parameter = optParamListN, Default = 10).  This procedure repeats until the fit of the model fails to improve.  This optimization routine accurately identified parameters on simulated data when extensively tested.
#'
#' @param fn A function that runs the model you would like to optimize.  It should return the fit statistic that is to be minimized.
#' @param parsUpper A list containing, for each parameter, a vector of values for the upper bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."
#' @param parsLower A list containing, for each parameter, a vector of values for the lower bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."
#' @param parsMinInt A list containing, for each parameter, a vector of values for the minimum interval sizes to be used when randomly choosing a value between the upper and lower bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."
#' @param otherParamList A list containing the other arguments/parameters to be passed to "fn."  The name of each element in the list must be the arguments/parameters name in "fn."
#' @param numLoops An integer that specifies the number of loops with different parameter values to be searched in each run of the smartGridSearch before the best fits for that run are identified. DEFAULT = 200.
#' @param numIntervals An integer that specifies the number of intervals that the range between the upper and lower bounds of each parameter space will be segmented into.   DEFAULT = 25.
#' @param optParamListN An integer that specifies the number of best fit runs that will be used to extract the new bounds for next set of grid search loops.  So, if optParamListN == 10, then the parameter values from the 10 best runs will be used to refine the upper and lower bounds of for each parameter. DEFAULT = 10.
#' @param optBoundLoops An integer that specifies the number of times to re-simulate the best fit run in order to get a mean and SD for the minimized fit statistic. If the mean fit statistic is no longer the best fit, the program will move to the next best fit run, and so on until the it identifies the best mean fit run. DEFAULT = 10.
#' @param roundPrecision An integer that specifies the number of digits that the fit statistic should be rounded to when comparing determining if the optimization is complete. DEFAULT = 3.
#' @param progress An boolean that specifies whether a progress bar should display for each run of the optimization. This works only when multicore = F. DEFAULT = TRUE.
#' @param multicore An boolean that specifies whether the program should be run using multiple processing cores. DEFAULT = FALSE.
#' @param multicorePackages An vector of strings specifying the package names that are used in "fn." This needs to be used when multicore = TRUE. DEFAULT = NULL.
#'
#' @return A list containing: (1) currentBest (the parameter values for the best fit) and (2) best10 (the best fit parameter values for the top optParamListN runs)
#'
#' @keywords optimize Grid Search Smart
#' @export
#' @importFrom foreach %dopar%
#' @examples smartGridSearch (assessRRWfit, parsUpper = list(a = (a1 = 1, a2 = 1)), parsLower = list(a = (a1 = 0, a2 = 0)), parsMinInt = list(a = (a1 = 0.01, a2 = 0.01)), otherParamList = list( data = data.frame ( ... ), ...), numLoops = 100, numIntervals = 100, optBoundLoops = 10, multicore = TRUE, multicorePackages = c('RRW'))

smartGridSearch <- function (fn, parsUpper, parsLower, parsMinInt, otherParamList = NULL, numLoops = 200, numIntervals = 25, optParamListN = 10, optBoundLoops = 10, roundPrecision = 3, progress = F, multicore = F, multicorePackages = NULL) {

    library(chutils)

    sinkFilename <- paste(format(Sys.time(), "%b_%d_%Y_%H-%M"), "gridLog.txt", sep="_")
    #output parameters to a file
    sink(sinkFilename, append = F)
      cat("\n\n parsUpper: \n\n")
      print(parsUpper)
      cat("\n\n parsLower: \n\n")
      print(parsLower)
      cat("\n\n parsMinInt: \n\n")
      print(parsMinInt)
      cat("\n\n numLoops: ", numLoops, "\n\n")
      cat("\n\n numIntervals: ", numIntervals, "\n\n")
      cat("\n\n optParamListN: ", optParamListN, "\n\n")
      cat("\n\n optBoundLoops: ", optBoundLoops, "\n\n")
      cat("\n\n roundPrecision: ", roundPrecision, "\n\n")
    sink(NULL)

    currentBest <- NULL
    done <- FALSE

    #get the parameter names
    parNames <- names(parsUpper)

    #copy the initial parameters to the variables that will change
    optParsLower <- parsLower
    optParsUpper <- parsUpper
    optParsInt <- parsMinInt

    #set up the progress bar
    if(progress) {
      width = ifelse(numLoops <= 100, numLoops, 100)
      pb <- txtProgressBar(1, numLoops, 1, style = 3, width=width)
    }

    #set-up multi-core processing
    if(multicore) {
      cl<- setUpParallel ()
    }

    #Do the following until criterion is reached
    while(done == FALSE) {

      df.pars <- list()
      #repeat the following. This does a random sampling of the parameter space
      #This is the non-multicore implementation
      if(multicore == FALSE) {
        for(j in 1:numLoops) {

          df.pars.tmp <- runSimulation(fn, optParsUpper, optParsLower, parsMinInt, otherParamList, numIntervals)

          #add the current run to the dataframe containing the previous runs
          df.pars <- c(df.pars, df.pars.tmp)

          #show progress
          if(progress) {
            setTxtProgressBar(pb, j)
          }
        }
      } else {
        #this is the multi-core implementation
          df.pars <- foreach::foreach(j=1:numLoops, .combine='c', .multicombine=TRUE, .export=c('runSimulation', 'getRandomNumber'), .packages=multicorePackages) %dopar% {

          runSimulation(fn, optParsUpper, optParsLower, parsMinInt, otherParamList, numIntervals)
        }
      }

      #after sampling the parameter space numLoops times, add the currentBest fit (from last sequence of loops if it exists) to the parameter list
      if(is.null(currentBest) == FALSE) {
        df.pars <- c(df.pars, list(currentBest))
      }
      #get the parameters of the 10 (optParamListN) best fits
      df.bestPars <- getOrderedParamList(df.pars, "r_out", optParamListN, fn, otherParamList, optBoundLoops)

      #make the best fit the "new best"
      newBest <- df.bestPars[[1]]

      #For each parameter, get the optimal bounds based on the 10 best fits
      for(i in 1:length(parNames)) {
        #first get the vector of values for the parameter[i]
        tmp.up <- parsUpper[[parNames[i]]]
        tmp.lo <- parsLower[[parNames[i]]]
        tmp.int <- parsMinInt[[parNames[i]]]
        opt.up <- NULL
        opt.lo <- NULL
        opt.int <- NULL
        #how many values in the vector?
        tmp.n <- length(tmp.up)
        #now get all the simulated values for that parameter for the best simulations
        tmp.best <- lapply(df.bestPars,'[', c(parNames[i]))
        #convert them into dataframe
        tmp.best.array <- as.data.frame(matrix(unlist(tmp.best), ncol=tmp.n, byrow=T))

        #for each column in the dataframe (i.e., each value in the vector - e.g., b1, b2, etc.), get the new bounds.
        for(j in 1:length(tmp.up)) {
          tmpPars <- getOptBounds(tmp.best.array[,j], tmp.up[j],tmp.lo[j], tmp.int[j],numIntervals)
          opt.lo[j] <- tmpPars["lower"]
          opt.up[j] <- tmpPars["upper"]
          opt.int[j] <- tmpPars["int"]
        }
        #add the new bounds to the optPars ...
        optParsLower[parNames[i]] <- list(opt.lo)
        optParsUpper[parNames[i]] <- list(opt.up)
        optParsInt[parNames[i]] <- list(opt.int)
      }

      #If the currentBest from the last run is less good the newBest, then assign the newBest to currentBest and run another loop
      #If the currentBest from the last run is as good or better than the newBest, then end the grid search and output the result
      if(is.null(currentBest) == FALSE) {
        if(round(newBest$r_out, roundPrecision)  < round(currentBest$r_out, roundPrecision)) {
          currentBest <- newBest
        } else {
          done <- TRUE
        }
      } else {
        currentBest <- newBest
      }
      print(currentBest$r_out)

      #output current best for the current sequence to a file
      sink(sinkFilename, append = T)
        cat("\n\n currentBest: \n\n")
        print(currentBest)
        cat("\n\n parsUpper: \n\n")
        print(optParsUpper)
        cat("\n\n parsLower: \n\n")
        print(optParsLower)
        cat("\n\n parsMinInt: \n\n")
        print(optParsInt)
      sink(NULL)
    }

    #end multi-core processing
    if(multicore) {
      endParallel (cl)
    }

    return(list(final = currentBest, best10 = df.bestPars))

}
