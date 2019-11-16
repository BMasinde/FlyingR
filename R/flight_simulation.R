#' @title Range Estimation
#' @description  Practical range estimation of birds using methods in Pennycuik (1975)
#' Mechanics of Flight. These methods are based on Breguet equations.
#'
#' @author Brian Masinde
#'
#' @name flysim
#' @param fileArgs Arguments for path to data. Same arguments as would be supplied
#'        to read.csv(), but as a list
#' @param data A data frame.
#' @param control A list for re-defining constants. See details.
#'
#' @details The option *control takes the folowing arguments
#' \itemize{
#'    \item ppc: Profile power constant
#'    \item eFat: Energy content of fuel from fat
#'    \item g: Accelaration due to gravity
#'    \item mce: Mechanical conversion efficiency [0,1]
#'    \item ipf: Induced power factor
#'    \item vcp: Ventilation and circulation power
#'    \item airDensity: Air density at cruising altitude
#'    \item bdc: Body drag coefficient
#'    \item alpha: Basal metabolism factors in passerines and non passerines
#'    \item delta: Basal metabolism factors in passerines and non passerines
#'    alpha*bodyMass^delta
#'}
#' @include misc_functions.R lookup_table2.R input_match.R method_1.R method_2.R
#' @return S3 class object with range estimates based on methods defined and
#'        constants
#' \itemize{
#'    \item range estimates in kilometre
#'    \item constants (list)
#'    \item
#' }
#'
#' @export flysim
#'
#' @examples
#' flysim(data = birds, control = list(energy = 3.89*10^7))
#' flysim(data = birds,  control = list(airDensity = 0.905))
#'
#' @usage flysim(data, control = list())


flysim <- function(fileArgs = list(), data, control = list()) {

  ##  Error check data #########################################################
  if (missing(fileArgs) == TRUE & missing(data) == TRUE) {
    stop("Data not found \n", call. = TRUE)
  }

  if (missing(fileArgs) == FALSE & missing(data) == FALSE) {
    stop("Both path and data given. Function needs only one of the two \n", call. = TRUE)
  }

  # if (is.data.frame(data) == FALSE & is.list(data) == FALSE) {
  #   stop(">> data as list or data frame <<")
  # }
  #
  # # check number of columns
  # if (is.data.frame(data) == TRUE && ncol(data) < 6) {
  #   stop(">> at least 5 columns for data <<")
  # }
  # # check number of fields
  # if (is.list(data) == TRUE && length(data) < 6) {
  #   stop("data list should have at least 4 fields")
  # }

  # column match
  if (missing(fileArgs) == FALSE) {
    data <- .pathToData(fileArgs, ...)
  } else if (missing(fileArgs) == TRUE) {
    data <- .colnames.match(data)
  }

  # control check
  if (missing(control) == TRUE) {
    cons <- .control()
  } else {
    cons <- .control(control)
  }


  results <- list("range" = vector(),
                  #"fuel" = vector(),
                  #"Vmp" = vector(),
                  #"Vmr" = vector(),
                  "constants" = cons,
                  "data" = data
                  )

  results$range <-ifelse( data$taxon == 1, .breguet( data$allMass, data$wingSpan,
        data$fatMass, data$taxon, data$wingArea, cons),
      .breguet_adj( data$allMass, data$wingSpan, data$fatMass, data$taxon,
                    data$wingArea, cons)
    )

  # results should be named vectors
  if (!is.null(data$name)) {
    names(results$range) <- as.vector(data$name)
  } else {
    names(results$range) <- as.vector(data$ID)
  }


  # return object of class flysim
  class(results) <- append(class(results), "flysim")
  # return class object
  return(results)
}



