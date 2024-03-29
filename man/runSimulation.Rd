% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runSimulation.r
\name{runSimulation}
\alias{runSimulation}
\title{Function to run the simulation for the smartGridSearch.}
\usage{
runSimulation(
  fn,
  optParsUpper,
  optParsLower,
  parsMinInt,
  otherParamList,
  numIntervals
)
}
\arguments{
\item{fn}{A function that runs the model you would like to optimize.  It should return the fit statistic that is to be minimized.}

\item{parsMinInt}{A list containing, for each parameter, a vector of values for the minimum interval sizes to be used when randomly choosing a value between the upper and lower bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."}

\item{otherParamList}{A list containing the other arguments/parameters to be passed to "fn."  The name of each element in the list must be the arguments/parameters name in "fn."}

\item{numIntervals}{An integer that specifies the number of intervals that the range between the upper and lower bounds of each parameter space will be segmented into.   DEFAULT = 25.}

\item{parsUpper}{A list containing, for each parameter, a vector of values for the upper bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."}

\item{parsLower}{A list containing, for each parameter, a vector of values for the lower bounds of that parameter's elements.  The name of the vector must correspond to the parameter name in the "fn."}
}
\value{
A list containing the parameter values for this run of the simulation and the fit statistic that is to be minimized
}
\description{
This function runs the simulation within the smartGridSearch optimization program.  It runs "fn" once and outputs the parameter values and the fit statistic.
}
\examples{
runSimulation (fn, optParsUpper = list(a = (a1 = 1, a2 = 1)), optParsLower = list(a = (a1 = 0, a2 = 0)), parsMinInt = list(a = (a1 = 0.01, a2 = 0.01)), otherParamList = list( data = data.frame ( ... ), ...), numIntervals = 25)
}
\keyword{Grid}
\keyword{Search}
\keyword{Smart}
\keyword{simulation}
