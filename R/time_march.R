#' @title  Renge estimation by constant specific work hypothesis
#' @author Brian Masinde
#' @name csw
#' @param data A dataframe of observations
#' @param control A list of flight control parameters
#' @include input_match.R misc_functions.R

csw <- function(data, control = list()) {

  if (missing(control)) {
    message("control missing, using default parameters")
  }

  # objects
  results <- list("data" = data,
                  "range" = vector(),
                  "fuel" = vector(),
                  "fatFrac" = vector() ,
                  "muscleFrac" = vector(),
                  "frameMass" = vector(),
                  "constants" = list()
  )

  # column match
  cols <- .colnames.match(names(data))

  # compute fractions
  fatFrac <- data[, cols$fatMass]/data[, cols$bodyMass]

  muscleFrac <- data[, cols$muscleMass]/data[, cols$bodyMass]

  results$frameMass <- data[, cols$bodyMass]*(1 - fatFrac - muscleFrac)

  results$fatFrac <- fatFrac

  results$muscleFrac <- muscleFrac

  cons <- list(
    # profile power constant
    ppcons = 8.4,
    # energy density of fuel per kg
    efat = 3.9 * 10 ^ 7,
    # energy density of dry protein
    eprot = 1.8 * 10 ^7,
    # accelaration due to gravity
    g = 9.81,
    # mechanical conversion efficiency  [0,1]
    n = 0.23,
    # induced power factor
    k = 1.20,
    # ventilation and circulation power (Tucker's data)
    R =  1.10,
    # air density at fligh height
    airDensity = 1.00,
    # body drag coefficient
    bdc = 0.10,
    # constant varies btw passerines and non-passerines
    alpha = c(6.25, 3.79),
    delta = c(0.724, 0.723),
    # consumption
    consume = 1
  )

  # return object of class flysim
  class(results) <- append(class(results), "csw")
  # return class object
  return(results)
}
