#' Method 1 on practical range calculation based on Breguets equations
#' Author: Brian Masinde
#'
#' @name breguet
#'
#' @param body_mass
#' @param wing_span
#' @param fat_mass
#' @param Order
#' @param aspect_ratio
#' @param ctrl
#'
#' @return List with range (in km), constants used and fat fraction
#' @include misc_functions.R
#'


breguet <- function(body_mass, wing_span, fat_mass, Order, wing_area, ctrl) {
  # ctrl list of user defined constants
  if (missing(ctrl) == F &&
      is.list(ctrl) == FALSE) {
    stop("ctrl must be a list")
  }

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

  # check ctrl ---------------------------------------------------------------------
  if (missing(ctrl)) {
    cat("ctrl not defined. Using default constants.\nDefault air_dens = 1.00 kg m^3 \n")

  } else {
    ext_args <- c(
      "ppcons",
      "energy",
      "g",
      "n",
      "k",
      "R",
      "air_dens",
      "flight_height",
      "alpha",
      "delta",
      "bdc"
    )
    # match ext_args to user provided
    given <- which(ext_args %in% names(ctrl) == TRUE)

    # extract names
    cons_giv <- ext_args[given]
    for (i in 1:length(cons_giv)) {
      cons[cons_giv[i]] <- ctrl[cons_giv[i]]
    }
  }



  # # check user defined constants------------------------------------------------------
  # ext_args <- c("ppcons", "energy", "g", "n", "k",
  #               "R", "air_density", "flight_height",
  #               "alpha", "delta", "bdc" )
  # # default constants
  # cons <- list(ppcons = 8.4, # profile power constant
  #              energy = 4 * 10^7, # eneryg content of fuel per kg
  #              g = 9.81, # accelaration due to gravity
  #              n = 0.23, # mechanical efficiency  [0,1]
  #              k = 1.20, # induced power factor
  #              R =  1.10, # ventilation and circulation power (Tucker's data)
  #              air_dens = air_dens,
  #              bdc = 0.10, # body drag coefficient
  #              #flight_height = flight_height,
  #              alpha = c(6.25, 3.79), # constant varies btw passerines and non-passerines
  #              delta = c(0.724, 0.723) # constant varies btw passerines and non-passerines
  # )

  # if (is.null(ctrl) == F) {
  #   # match ext_args to user provided
  #   given <- which(ext_args %in% names(ctrl) == TRUE)
  #
  #   # extract names
  #   cons_giv <- ext_args[given]
  #
  #   # update constants in a loop
  #     for (i in 1:length(cons_giv)) {
  #       cons[cons_giv[i]] <- ctrl[cons_giv[i]]
  #     }
  #
  # }


  # fat fraction------------------------------------------------------------------------
  fat_frac <- fat_mass/body_mass

  x2 <- met_pow_ratio(cons, body_mass, wing_span)

  # x1:ppcons/Aspect ratio + x2:mpratio check for D ----------------------------------
  # Aspect ratio = wing_span^2 / wing_area
  # D is the effective drag force found by interpolation (table 2)
  # add ppratio to x2 and interpolate
  # round off to 2 digits

  D <- sapply(round((prof_pow_ratio(ws = wing_span, wa = wing_area, cons) + x2), 2),
              function(x)
                table2$D[which(table2$x1plusx2 >= x)[1]])

  ### Ask if we should round off when interpolating

  # Effective lift:drag ratio---------------------------------------------------------
  # Disk area Sd
  Sd <- 0.25 * pi * (wing_span ^ 2)

  # flat-plate area
  A <- 0.00813 * (body_mass ^ 0.666) * cons$bdc

  # lift drag ratio at begining of flight
  eldr1 <- (D / ((cons$k ^ 0.5) * cons$R)) * ((Sd / A) ^ 0.5)

  # increase by 10F%
  corr_change <- eldr1 + (eldr1 * (10 * fat_frac) / 100)

  # range Y ------------------------------------------------------------------------
  Y <-
    ((cons$energy * cons$n) / cons$g) * corr_change * log(1 / (1 - fat_frac))/1000

  # return list of objects ---------------------------------------------------------

  obj <- list("Range" = Y,
              "constants" = cons,
              "fuel" = fat_frac)

  return(obj)
}

# # test function
# system.time({
#   breguet(body_mass = body_mass, wing_span = wing_span,
#           fat_mass = fat_mass, Order = Order, aspect_ratio = aspect_ratio, ctrl = list(air_dens = 1.11)
#   )
# })
breguet(body_mass = body_mass, wing_span = wing_span,
        fat_mass = fat_mass, Order = Order, wing_area = wing_area
        )

