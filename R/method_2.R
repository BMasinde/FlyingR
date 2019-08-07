#' Method 2 practical range calculation based on Breguets equations with mean
#'  of effective lift: drag ratio
#'
#' @author Brian Masinde
#'
#' @name breguet_adj
#'
#' @param body_mass All up mass. Including fuel, crop contents and equipment in Kg
#' @param wing_span Wing span in metres
#' @param fat_mass Fat mass in kg (fuel)
#' @param Order Passerine (1) or non-passerine (2)
#' @param aspect_ratio Wing span squared divided by wing area
#' @param consumption Percentage of fuel to be consumed by end of flight. Between 0-1.
#' @param ctrl If need be to redefine constants such as air density
#'
#'

breguet_adj <- function(body_mass, wing_span, fat_mass, Order, aspect_ratio,  consumption, ctrl) {
  # ctrl list of user defined constants
  if (missing(ctrl) == F &&
      is.list(ctrl) == FALSE) {
    stop("ctrl must be a list")
  }

  if (missing(consumption) == T) {
    # use all fuel
    consumption <- 1
  } else if (consumption < 0 ||
             consumption > 1) {
    stop("Consumption value between 0 and 1")
  }

  # if (consumption < 0 ||
  #     consumption > 1) {
  #   stop("Consumption value between 0 and 1")
  # }



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
    cat("ctrl not defined. \nUsing default constants.\nDefault air_dens = 1.00 kg m^3")

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



  # fat fraction------------------------------------------------------------------------
  fat_frac <- fat_mass/body_mass

  # m2 mass end of flight---------------------------------------------------------------
  body_mass_end <- body_mass - (fat_mass * consumption)



  # x2 calculation ---------------------------------------------------------------------

  x2_end <- met_pow_ratio(cons, body_mass_end)

  # x1:ppcons/Aspect ratio + x2:mpratio check for D ----------------------------------
  # Aspect ratio = wing_span^2 / wing_area
  # D is the effective drag force found by interpolation (table 2)
  # add ppratio to x2 and interpolate
  # round off to 2 digits

  D_end <- sapply(round((cons$ppcons / (wing_span^2/wing_area)) + x2_end, 2),
              function(x)
                table2$D[which(table2$x1plusx2 >= x)[1]])

  ### Ask if we should round off when interpolating

  # Effective lift:drag ratio---------------------------------------------------------
  # Disk area Sd
  Sd <- 0.25 * pi * (wing_span ^ 2)

  # flat-plate area
  A_end <- 0.00813 * (body_mass_end ^ 0.666) * cons$bdc

  # lift drag ratio at begining of flight
  eldr_end <- (D_end / ((cons$k ^ 0.5) * cons$R)) * ((Sd / A_end) ^ 0.5)


  # repeat begining of flight ------------------------------------------------------
  x2_start <- x2_end / ((1 / (1 - fat_frac)) ^ 1.75)

  D_start <- sapply(round((cons$ppcons / (wing_span^2/wing_area)) + x2_start, 2),
                  function(x)
                    table2$D[which(table2$x1plusx2 >= x)[1]])


  eldr_start <-
    (D_start / ((cons$k ^ 0.5) * cons$R)) * (((Sd / A_end) ^ 0.5) / ((body_mass /
                                                                        body_mass_end) ^ 0.5))


  # range Y ------------------------------------------------------------------------
  Y <-
    ((cons$energy * cons$n) / cons$g) * apply(cbind(eldr_start, eldr_end), 1, mean) * log(1 / (1 - fat_frac)) /
    1000

  # return list of objects ---------------------------------------------------------

  obj <- list("Range" = Y,
              "constants" = cons,
              "fuel" = fat_frac)

  return(obj)
}

breguet_adj(body_mass = body_mass, wing_span = wing_span,
        fat_mass = fat_mass, Order = Order, aspect_ratio = aspect_ratio, ctrl = list(air_dens = 1.11)
)

