#' Function to resize the upper and lower boundary range of a parameter.
#'
#' This function resize the upper and lower boundary range of a parameter based on the best fit runs of the simulation.  The resizing is done so that it iteratively narrows down to the best parameter values.
#'
#' @param arrayIn An array of the the parameter's values from the best fit runs (presumably).
#' @param originalUpperBound An number that specifies the original, unaltered upper bound of the parameter that was input at the start of smartGridSearch.
#' @param originalLowerBound An number that specifies the original, unaltered lower bound of the parameter that was input at the start of smartGridSearch.
#' @param minimumIntervalValue An number that specifies the original, unaltered minimum interval for the parameter that was input at the start of smartGridSearch.
#' @param numIntervals An integer that specifies the number of intervals that the range between the upper and lower bounds of each parameter space will be segmented into.   DEFAULT = 25.
#'
#' @return An array of three values: "lower" = the new lower bound; "upper" = the new upper bound; "int" = the new interval value.
#'
#' @keywords optimal bounds
#' @export
#' @examples getOptBounds(arrayIn = c(1,5,3,5,3), originalUpperBound = 10, originalLowerBound = 0, minimumIntervalValue = 0.1, numIntervals=25)

getOptBounds <- function (arrayIn,originalUpperBound, originalLowerBound, minimumIntervalValue, numIntervals=25) {

		 bestVal <- arrayIn[1]

		 rangeValsR <- max(arrayIn) - min(arrayIn)

		 upperDist <- max(arrayIn) - bestVal
		 lowerDist <- bestVal - min(arrayIn)

		 minVal <- bestVal - lowerDist
		 maxVal <- bestVal + upperDist

		if (minVal < originalLowerBound) minVal <- originalLowerBound
		if (maxVal > originalUpperBound) maxVal <- originalUpperBound

		intVal <- (maxVal - minVal)/numIntervals

    #make sure the interval value is not smaller than the minimum Interval Value
    intVal <- as.numeric(ifelse(intVal < minimumIntervalValue, minimumIntervalValue, intVal))

    return (c(lower = minVal, upper = maxVal, int = intVal))
}
