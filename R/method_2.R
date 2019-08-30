#' Method 2 practical range calculation based on Breguets equations with mean
#'  of effective lift: drag ratio
#'
#' @author Brian Masinde
#'
#' @name breguet_adj
#'
#' @param bodyMass All up mass. Including fuel, crop contents and equipment in Kg
#' @param wingSpan Wing span in metres
#' @param fatMass Fat mass in kg (fuel)
#' @param ordo Passerine (1) or non-passerine (2)
#' @param wingArea Wing area
#' @param consumption Percentage of fuel to be consumed by end of flight. Between 0-1.
#' @param ctrl If need be to redefine constants such as air density
#'
#' @include misc_functions.R lookup_table2.R

.breguet_adj <- function(bodyMass, wingSpan, fatMass, ordo, wingArea, consumption, ctrl) {
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
    extArgs <- c(
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
    # match extArgs to user provided
    given <- which(extArgs %in% names(ctrl) == TRUE)

    # extract names
    consGive <- extArgs[given]
    for (i in 1:length(consGive)) {
      cons[consGive[i]] <- ctrl[consGive[i]]
    }
  }



  # fat fraction
  fatFrac <- fatMass/bodyMass

  # m2 mass end of flight
  bodyMassEnd <- bodyMass - (fatMass * consumption)

  # x2
  metPowRatioEnd <- .met.pow.ratio(cons, bodyMassEnd)

  # x1:ppcons/Aspect ratio + x2:mpratio check for D ----------------------------------
  # Aspect ratio = wingSpan^2 / wingArea
  # D is the effective drag force found by interpolation (table 2)
  # add ppratio to x2 and interpolate
  # round off to 2 digits

  dragEnd <- sapply(round((cons$ppcons / (wingSpan^2/wingArea)) + metPowRatioEnd, 2),
              function(x)
                table2$D[which(table2$x1plusx2 >= x)[1]])

  ### Ask if we should round off when interpolating

  # Effective lift:drag ratio---------------------------------------------------------
  # Disk area diskArea
  diskArea <- 0.25 * pi * (wingSpan ^ 2)

  # flat-plate area
  flatPlateAreaEnd <- 0.00813 * (bodyMassEnd ^ 0.666) * cons$bdc

  # lift drag ratio at begining of flight
  liftDragEnd <- (dragEnd / ((cons$k ^ 0.5) * cons$R)) * ((diskArea / flatPlateAreaEnd) ^ 0.5)


  # repeat begining of flight ------------------------------------------------------
  # why not calculate metPowRatioStart using the funciton but with bodymass at start
  metPowRatioStart <- metPowRatioEnd / ((1 / (1 - fatFrac)) ^ 1.75)

  dragStart <- sapply(round((cons$ppcons / (wingSpan^2/wingArea)) + metPowRatioStart, 2),
                  function(x)
                    table2$D[which(table2$x1plusx2 >= x)[1]])


  liftDragStart <-
    (dragStart / ((cons$k ^ 0.5) * cons$R)) * (((diskArea / flatPlateAreaEnd) ^ 0.5) / ((bodyMass /
                                                                        bodyMassEnd) ^ 0.5))


  # range in kilometres ------------------------------------------------------------------------
  kmRange <-
    ((cons$energy * cons$n) / cons$g) * apply(cbind(liftDragStart, liftDragEnd), 1, mean) *
    log(1 / (1 - fatFrac)) / 1000
  # power curve --------------------------------------------------------------------
  pc <- .pow.curve(bodyMass, wingSpan, wingArea)
  # return list of objects ---------------------------------------------------------

  results <- list("Range" = kmRange,
              "constants" = cons,
              "fuel" = fatFrac,
              "Vmp" = pc[[1]],
              "Vmr" = pc[[2]])

  return(results)
}

