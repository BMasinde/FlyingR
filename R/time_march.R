#' An attempt at range estimation using time marching
#'
#' @author Brian Masinde
#'
#'
#'


time_march <- function(body_mass, wing_span, wing_area) {

  # default constants
  cons <- list(
    # profile power constant
    ppcons = 8.4,

    # eneryg content of fuel per kg
    energy = 4 * 10 ^ 7,

    # accelaration due to gravity
    g = 9.81,
    # mechanical efficiency  [0,1]
    n = 0.23,

    # induced power factor
    k = 1.20,

    # ventilation and circulation power (Tucker's data)
    R =  1.10,

    # air density at fligh height
    air_dens = 1.00,

    # body drag coefficient
    bdc = 0.10,

    # constant varies btw passerines and non-passerines
    alpha = c(6.25, 3.79),
    delta = c(0.724, 0.723)
  )

  # start with power curve

  # flight calculates mechanical power first then estimates chemical power (pg 47, 53)

  # Estimating the minimum power speed pg 66------------------------------------------------


  # Sb = bfa = body frontal area
  min_pow_speed <-
    ((0.807 * cons$k ^ 0.25 * body_mass ^ 0.5 * cons$g ^ 0.5) /
    (cons$air_dens ^ 0.5 * wing_span ^ 0.5 * bfa(body_mass) ^ 0.25 * cons$bdc ^
       0.25)) - 0.9 # speed slightly lower

  # induced power in horizontal flight Box 3.1 eqn 16




}
