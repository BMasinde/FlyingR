#' @title Range Estimation
#' @description  Practical range estimation of birds using methods in Pennycuik (1975)
#' Mechanics of Flight. These methods are based on Breguet equations.
#'
#' @author Brian Masinde
#'
#' @name migrate
#'
#' @param data A data frame.
#' @param path Path to data csv file
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
#'    \item data as a data frame
#'    \item range estimates
#'    \item fuel
#'    \item constants (list)
#' }
#'
#' @importFrom dplyr filter
#'
#' @export migrate
#'
#' @examples
#' migrate(data = birds, ctrl = list(energy = 3.89*10^7))
#' migrate(data = birds,  ctrl = list(airDensity = 0.905))
#'
#' @usage migrate(data, ctrl = list())
#'


migrate <- function(path, data, control, method = "cmm") {

  if (missing(path) == TRUE & missing(data) == TRUE) {
    stop("Data not found", call. = TRUE)
  }



}

