#' Method 1 on practical range calculation based on Breguets equations
#'
#' @author Brian Masinde
#' @param bodyMass all up mass
#' @param wingSpan wing span of bird in metres
#' @param fatMass fat mass of bird
#' @param ordo Passerine (1) or non-passerine (2)
#' @param wingArea area of wing
#' @param ctrl A list of re-definition of constants (i.e *airDensity*,
#'             *consume*, *enegry e*, *mechanical efficiency n*).
#' @importFrom utils tail
#' @return List with range (in km), constants used and fat fraction
#' @include misc_functions.R lookup_table2.R
#'


.breguet <- function(bodyMass, wingSpan, fatMass, ordo, wingArea, ctrl) {

  ##############################################################################
  # ctrl list of user defined constants
  if (missing(ctrl) == F &&
      is.list(ctrl) == FALSE) {
    stop("ctrl must be a list")
  }

  # non-zero fat mass
  if (length(which(fatMass == 0)) != 0) {
    stop("In Method breguet, empty fat mass.")
  }

  #############################################################################
  # default constants
  cons <- list(
    # profile power constant
    ppcons = 8.4,

    # energy content of fuel per kg
    energy = 4 * 10 ^ 7,

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
    delta = c(0.724, 0.723)
  )

  ##############################################################################
  # check ctrl
  if (missing(ctrl)) {
    message("## ctrl not defined. Using default constants.
            \nDefault air_dens = 1.00 kg m^3 \n")

  } else {
    extArgs <- c(
      "ppcons",
      "energy",
      "g",
      "n",
      "k",
      "R",
      "airDensity",
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

  ##############################################################################
  # fat fraction
  fatFrac <- fatMass/bodyMass

  # metabolic power ratio metPowRatio
  metPowRatio <- .met.pow.ratio(cons, bodyMass, wingSpan, ordo)

  # x1:ppcons/Aspect ratio + metPowRatio:mpratio check for Drag
  # Aspect ratio = wingSpan^2 / wingArea
  # drag is the effective drag force found by interpolation (table 2)
  # add ppratio to metPowRatio and interpolate
  # round off to 2 digits
  table2 <- .gen.table2()

  dFactor <-
    sapply(round((
      .prof.pow.ratio(ws = wingSpan, wa = wingArea, cons) + metPowRatio
    ),
    2), .interpolate, table2)


  ##############################################################################
  # Effective lift:drag ratio
  # Disk area diskArea
  diskArea <- 0.25 * pi * (wingSpan ^ 2)

  # flat-plate area
  flatPlateArea <- 0.00813 * (bodyMass ^ 0.666) * cons$bdc

  # lift drag ratio at begining of flight
  liftDragRatio <- (dFactor / ((cons$k ^ 0.5) * cons$R)) *
    ((diskArea / flatPlateArea) ^ 0.5)

  # increase by 10F%
  liftDragRatio <- liftDragRatio + (liftDragRatio * (10 * fatFrac) / 100)

  # range in kilometres
  kmRange <-
    ((cons$energy * cons$n) / cons$g) * liftDragRatio *
    log(1 / (1 - fatFrac))/1000

  ##############################################################################
  # power curve
  pc <- .pow.curve(bodyMass, wingSpan, wingArea, cons)

  # return list of objects

  results <- list("Range" = kmRange,
              "constants" = cons,
              "fuel" = fatFrac,
              "Vmp" = pc[[1]],
              "Vmr" = pc[[2]]
              )

  return(results)
}

################################################################################

