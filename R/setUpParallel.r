#' Function that initiates the necessary steps so that one can use "foreach" to run on multiple cores
#'
#' This function initiates the necessary steps so that one can use "foreach" to run on multiple cores
#'
#' @param No parameters.
#'
#' @return on object of class "cluster"
#'
#' @keywords foreach parallel set up
#' @export
#' @examples setUpParallel ()

setUpParallel <- function () {

		cores <- parallel::detectCores()
		cl <- parallel::makeCluster(cores, outfile='log.txt')
	  doParallel::registerDoParallel(cl)

  print(paste("Parallel Setup: Number of Cores = ", cores, sep=": "))
	return (cl)
}

#' Function that completes the necessary steps to stop the parallel cluster started with setUpParallel()
#'
#' This function completes the necessary steps to stop the parallel cluster started with setUpParallel()
#'
#' @param c1 A "cluster" object that was returned by setUpParallel().
#'
#' @return .
#'
#' @keywords foreach parallel stop
#' @export
#' @examples endParallel (c1)

endParallel <- function (cl) {
	parallel::stopCluster(cl)
	rm(cl)
	print("cluster stopped")
}
