#' Function that returns "n" random numbers between an upper and lower bound.
#'
#' This Function that returns "n" random numbers between an upper and lower bound, with a specific interval.
#'
#' @param n An integer specifying the number of values to return.
#' @param min A number specifying the lower bound of the range to draw from.
#' @param max A number specifying the upper bound of the range to draw from.
#' @param interval A number specifying the size of the steps to take between the upper and lower bounds.
#'
#' @return n values between min and max, with an interval.
#'
#' @keywords random number min max
#' @export
#' @examples getRandomNumber (1, 0, 10, 1)

getRandomNumber <- function (n, min, max, interval) {
	x <- sample(rep(seq (min,max,interval),2),n, replace = TRUE)
	return (x)
}
