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
#' @param ctrl Re-define constants if better estimates exist. (*airDensity*,
#'             *consume*, *enegry e*, *mechanical efficiency n*).
#' @include misc_functions.R lookup_table2.R
#' @description Practical range estimation using Breguet equation for fixed wing
#'              with crude adjustments. Mean lift:drag ratio between start and
#'              end of flight is used as proxy for engine burn.


.breguet_adj <- function(bodyMass, wingSpan, fatMass, ordo, wingArea, ctrl) {
  ##############################################################################
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
    airDensity = 1.00,

    # body drag coefficient
    bdc = 0.10,

    # constant varies btw passerines and non-passerines
    alpha = c(6.25, 3.79),
    delta = c(0.724, 0.723)
  )

  ##############################################################################
  if (missing(ctrl) == T)  {
    message("## ctrl not defined. Using default constants. ## \n")
    # use all fuel
    consume <- 1

  } else if (missing(ctrl) == F &&
      is.list(ctrl) == FALSE) {
    stop("ctrl must be a list")
  } else if(!missing(ctrl) && is.null(ctrl[["consume"]]) == T) {
    # use all fuel
    consume  <- 1
    message("## 100% fuel consumption during flight ## \n")
  } else if(!missing(ctrl) && ctrl$consume < 0 || ctrl$consume > 1){
    stop("In ctrl, consume adhere [0,1]")
  } else if(!missing(ctrl)) {
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

  # if (is.null(ctrl[["consume"]]) == T) {
  #   # use all fuel
  #   consume  <- 1
  #   message("## 100% fuel consumption during flight ## \n")
  # } else if (ctrl$consume  < 0 ||
  #            ctrl$consume  > 1) {
  #   stop("consume value between 0 and 1")
  # }


  # default constants


  # # check ctrl -----------------------------------------------------------------
  # if (missing(ctrl)) {
  #   message("ctrl not defined. \nUsing default constants.
  #           \nDefault air_dens = 1.00 kg m^3")
  #
  # } else {
  #   extArgs <- c(
  #     "ppcons",
  #     "energy",
  #     "g",
  #     "n",
  #     "k",
  #     "R",
  #     "air_dens",
  #     "alpha",
  #     "delta",
  #     "bdc"
  #
  #   )
  #   # match extArgs to user provided
  #   given <- which(extArgs %in% names(ctrl) == TRUE)
  #
  #   # extract names
  #   consGive <- extArgs[given]
  #   for (i in 1:length(consGive)) {
  #     cons[consGive[i]] <- ctrl[consGive[i]]
  #   }
  # }



  # fat fraction
  fatFrac <- fatMass/bodyMass

  ## lift:drag end of flight ###################################################
  # m2 mass end of flight
  bodyMassEnd <- bodyMass - (fatMass * consume )

  # x2
  metPowRatioEnd <- .met.pow.ratio(cons, bodyMassEnd, wingSpan, ordo)

  # x1:ppcons/Aspect ratio + x2:mpratio check for D ----------------------------
  # Aspect ratio = wingSpan^2 / wingArea
  # D is the effective drag force found by interpolation (table 2)
  # add ppratio to x2 and interpolate
  # round off to 2 digits
  table2 <<- .gen.table2()

  dFactorEnd <- sapply(round((cons$ppcons / (wingSpan^2/wingArea)) +
                               metPowRatioEnd, 2), .interpolate)

  ### Ask if we should round off when interpolating

  # disk area
  diskArea <- 0.25 * pi * (wingSpan ^ 2)

  # flat-plate area
  flatPlateAreaEnd <- 0.00813 * (bodyMassEnd ^ 0.666) * cons$bdc

  # lift drag ratio at begining of flight
  liftDragEnd <- dFactorEnd / (cons$k ^ 0.5 * cons$R) * ((diskArea / flatPlateAreaEnd) ^ 0.5)


  ## lift:drag ratio start of flight ###########################################
  # why not calculate metPowRatioStart using the funciton but with bodymass at
  # start
  metPowRatioStart <- metPowRatioEnd / ((1 / (1 - fatFrac)) ^ 1.75)

  dFactorStart <- sapply(round((cons$ppcons / (wingSpan^2/wingArea)) +
                                 metPowRatioStart, 2), .interpolate)


  liftDragStart <-
    (dFactorStart / ((cons$k ^ 0.5) * cons$R)) *
    (((diskArea / flatPlateAreaEnd) ^ 0.5) / ((bodyMass / bodyMassEnd) ^ 0.5))


  ## Range in km ###############################################################
  kmRange <-
    ((cons$energy * cons$n) / cons$g) *
    apply(cbind(liftDragStart, liftDragEnd), 1, mean) *
    log(1 / (1 - fatFrac)) / 1000

  # Power curve
  pc <- .pow.curve(bodyMass, wingSpan, wingArea, cons)

  ## Results ###################################################################

  results <- list("Range" = kmRange,
              "constants" = cons,
              "fuel" = fatFrac,
              "Vmp" = pc[[1]],
              "Vmr" = pc[[2]])

  return(results)
}

