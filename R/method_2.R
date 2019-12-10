# Method 2 practical range calculation based on Breguets equations with mean
#  of effective lift: drag ratio
# @author Brian Masinde
# @param bodyMass All up mass. Including fuel, crop contents and equipment in Kg
# @param wingSpan Wing span in metres
# @param fatMass Fat mass in kg (fuel)
# @param ordo Passerine (1) or non-passerine (2)
# @param wingArea Wing area
# @param ctrl A list of re-definition of constants (i.e *airDensity*,
#             *consume*, *enegry e*, *mechanical efficiency n*).
# @importFrom utils tail
# @include misc_functions.R lookup_table2.R
# @description Practical range estimation using Breguet equation for fixed wing
#              with crude adjustments. Mean lift:drag ratio between start and
#              end of flight is used as proxy for engine burn.

#' @importFrom utils tail
.breguet_adj <- function(bodyMass, wingSpan, fatMass, ordo, wingArea, ctrl) {

  ##############################################################################
  # if (missing(ctrl) == T)  {
  #   message(">> ctrl not defined. Using default constants. << \n")
  # } else if (missing(ctrl) == F &&
  #     is.list(ctrl) == FALSE) {
  #   stop("ctrl must be a list", call. = FALSE)
  # }
  #
  # # non-zero fat mass
  # if (length(which(fatMass == 0)) != 0) {
  #   stop("In Method breguet, empty fat mass.", call. = FALSE)
  # }
  #
  # # if (sum(levels(ordo) == levels(factor(c(1, 2)))) != 2) {
  # #   stop("Order column should be a factor with levels 1 or 2", call. = FALSE)
  # # }
  #
  # ##############################################################################
  # # Assumptions
  # cons <- list(
  #   # profile power constant
  #   ppcons = 8.4,
  #
  #   # eneryg content of fuel per kg
  #   energy = 4 * 10 ^ 7,
  #
  #   # accelaration due to gravity
  #   g = 9.81,
  #   # mechanical efficiency  [0,1]
  #   n = 0.23,
  #
  #   # induced power factor
  #   k = 1.20,
  #
  #   # ventilation and circulation power (Tucker's data)
  #   R =  1.10,
  #
  #   # air density at fligh height
  #   airDensity = 1.00,
  #
  #   # body drag coefficient
  #   bdc = 0.10,
  #
  #   # constant varies btw passerines and non-passerines
  #   alpha = c(6.25, 3.79),
  #   delta = c(0.724, 0.723),
  #   # consumption
  #   consume = 1
  # )
  #
  # # user defined
  # if (missing(ctrl) == F) {
  #   extArgs <- c(
  #     "ppcons",
  #     "energy",
  #     "g",
  #     "n",
  #     "k",
  #     "R",
  #     "airDensity",
  #     "alpha",
  #     "delta",
  #     "bdc",
  #     "consume"
  #   )
  #
  #   # match extArgs to user provided
  #   given <- which(extArgs %in% names(ctrl) == TRUE)
  #
  #   # extract names
  #   consGive <- extArgs[given]
  #   for (i in 1:length(consGive)) {
  #     cons[consGive[i]] <- ctrl[consGive[i]]
  #   }
  #
  #   if(length(cons) > 11){
  #     stop("Wrong argument in ctrl", call. = FALSE)
  #   }
  #
  #   if(length(cons$delta) != 2 || length(cons$alpha) != 2) {
  #     stop("In ctrl, alpha and delta as vectors of length == 2", call. = FALSE)
  #   }
  #
  #   if(cons$consume < 0 || cons$consume > 1) {
  #     stop("In ctrl, consume adhere [0,1]", call. = FALSE)
  #   }
  # }


  ##############################################################################
  # fat fraction
  fatFrac <- fatMass/bodyMass

  ## lift:drag end of flight ###################################################
  # m2 mass end of flight
  #bodyMassEnd <- bodyMass - (fatMass * cons$consume)
  bodyMassEnd <- bodyMass - fatMass

  # x2
  metPowRatioEnd <- .met.pow.ratio(cons, bodyMassEnd, wingSpan, ordo)

  # x1:ppcons/Aspect ratio + x2:mpratio check for D ----------------------------
  # Aspect ratio = wingSpan^2 / wingArea
  # D is the effective drag force found by interpolation (table 2)
  # add ppratio to x2 and interpolate
  # round off to 2 digits
  table2 <- .gen.table2()

  # dFactorEnd <- sapply(round((cons$ppcons / (wingSpan^2/wingArea)) +
  #                              metPowRatioEnd, 2), .interpolate, table2)
  dFactorEnd <-
    sapply(round((
      .prof.pow.ratio(ws = wingSpan, wa = wingArea, cons) + metPowRatioEnd
    ),
    2), .interpolate, table2)

  # dFactorEnd <- sapply(round(1.2 +
  #                              metPowRatioEnd, 2), .interpolate, table2)

  ### Ask if we should round off when interpolating

  # disk area
  diskArea <- 0.25 * pi * (wingSpan ^ 2)

  # flat-plate area
  flatPlateAreaEnd <- 0.00813 * (bodyMassEnd ^ 0.666) * cons$bdc

  # lift drag ratio at begining of flight
  liftDragEnd <- dFactorEnd / (cons$ipf ^ 0.5 * cons$vcp) * ((diskArea / flatPlateAreaEnd) ^ 0.5)


  ## lift:drag ratio start of flight ###########################################
  # why not calculate metPowRatioStart using the funciton but with bodymass at
  # start
  metPowRatioStart <- metPowRatioEnd / ((1 / (1 - fatFrac)) ^ 1.75)

  # dFactorStart <- sapply(round((cons$ppcons / (wingSpan^2/wingArea)) +
  #                                metPowRatioStart, 2), .interpolate, table2)

  dFactorStart <-
    sapply(round((
      .prof.pow.ratio(ws = wingSpan, wa = wingArea, cons) + metPowRatioStart
    ),
    2), .interpolate, table2)

  # dFactorStart <- sapply(round(1.2 +
  #                                metPowRatioStart, 2), .interpolate, table2)

  liftDragStart <-
    (dFactorStart / ((cons$ipf ^ 0.5) * cons$vcp)) *
    (((diskArea / flatPlateAreaEnd) ^ 0.5) / ((bodyMass / bodyMassEnd) ^ 0.5))


  ## Range in km ###############################################################
  kmRange <-
    ((cons$eFat * cons$mce) / cons$g) *
    apply(cbind(liftDragStart, liftDragEnd), 1, mean) *
    log(1 / (1 - fatFrac)) / 1000

  return(round(kmRange, 1))
}

