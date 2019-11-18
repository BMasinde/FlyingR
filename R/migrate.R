#' @title Range Estimation
#' @description  Practical range estimation of birds using methods in Pennycuik (1975)
#' Mechanics of Flight. These methods are based on Breguet equations.
#'
#' @author Brian Masinde
#'
#' @name migrate
#'
#' @param data A data frame.
#' @param fileArgs Arguments for path to data
#' @param control A list for re-defining constants. See details.
#' @param method Methods for fuel management
#' @param speed_control One of two speed control methods. By default
#'        \textit{constant_speed} is used. \textit{vvmp_constant} is the alternative.
#'        The former holds the true airspeed constant while the latter holds the
#'        ratio of true airspeed and minimum power speed constant.
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
#'    \item data as a data frame
#'    \item range estimates
#'    \item fuel
#'    \item constants (list)
#' }
#'
#' @import Rcpp
#' @export migrate
#'
#' @examples
#' migrate(data = birds, ctrl = list(energy = 3.89*10^7))
#' migrate(data = birds,  ctrl = list(airDensity = 0.905))
#'
#' @usage migrate(data, ctrl = list())
#'


migrate <- function(fileArgs = list(), data, control = list(), method = "cmm",
                    speed_control = "constant_speed", ...) {

  if (missing(fileArgs) == TRUE & missing(data) == TRUE) {
    stop("Data not found \n", call. = TRUE)
  }

  if (missing(fileArgs) == FALSE & missing(data) == FALSE) {
    stop("Both path and data given. Function needs only one of the two \n", call. = TRUE)
  }

  if(speed_control != "constant_speed" | speed_control != "vvmp_constant") {
    stop("Wrong speed control  argument", call. = TRUE)
  }

  if(missing(fileArgs) == FALSE) {
    data <- .pathToData(fileArgs, ...)
  }


}

