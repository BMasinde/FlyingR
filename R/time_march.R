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
    # inverse power density of mitochondria
    invPowMit = 1.1 * 10 ^-6,
    # ratio V:Vmp
    vvmpRatio = 1.2,
    # density of muscle
    muscDensity = 1060,
    # consumption
    consume = 1,
    # protein hydration ratio
    hprot = 2.2
  )

  # column match
  cols <- .colnames.match(names(data))

  allMass <- data[, cols$bodyMass]
  fatMass <- data[, cols$fatMass]
  muscMass <- data[, cols$muscleMass]
  wingSpan <- data[, cols$wingSpan]
  wingArea <- data[, cols$wingArea]
  taxon <- data[, cols$order]


  # compute fractions
  fatFrac <- data[, cols$fatMass]/data[, cols$bodyMass]

  muscleFrac <- data[, cols$muscleMass]/data[, cols$bodyMass]

  frameMass <- data[, cols$bodyMass]*(1 - fatFrac - muscleFrac)


  # wing beat frequency
  wingFreq <- .wingbeat.freq(m = data[, cols$bodyMass], g = cons$g,
                      ws = data[, cols$wingSpan], wa = data[, cols$wingArea],
                      rho = cons$airDensity
                      )
  # minimum power speed
  vmp <- .min.pow.speed(m = data[, cols$bodyMass],
                        ws = data[, cols$wingSpan],
                        cons = cons)

  # total mechanical power
  pmech <- .total.mech.power(m = data[, cols$bodyMass],
                             ws = data[, cols$wingSpan],
                             wa = data[, cols$wingArea],
                             Vt = vmp*cons$vvmpRatio, cons = cons)

  # convert mechanical power to chemical
  rateCons <- (pmech/cons$n)

  rateMassDep <- rateCons/cons$efat


  pchem <-
    (rateCons * 0.1 + rateCons) +
    .basal.met(cons, mFrame = frameMass, mMusc = muscMass, ordo = taxon)

  # simulation for song thrush
  range <- vector()
  pmech <- vector(); #power <- pchem[2]
  pchem <- vector() # power
  rConsumeFuel <- vector() # rate of consumption of fuel energy
  fmass <- vector() ; fmass <- fatMass
  rateMassDep <- vector()
  specWork <- vector(); #specWork <- pmech[2]/(muscMass[2] * wingFreq[2])
  speed <- vector(); #speed[1] <- vmp[2] * cons$vvmp_ratio
  mmass <- vector(); #mmass[1] <- muscMass[2]
  fwing <- vector(); #fwing[1] <- wingFreq[2]

  i <- 1
  while (fmass[i] > 0) {
    # find speed and power
    pmech[i] <- .total.mech.power(m = data[, cols$bodyMass],
                               ws = data[, cols$wingSpan],
                               wa = data[, cols$wingArea],
                               Vt = vmp*cons$vvmpRatio, cons = cons)
    rConsumeFuel[i] <- (pmech/cons$n)

    rateMassDep <- rateCons/cons$efat

    pchem[i] <-
      (rConsumeFuel * 0.1 + rateCons) +
      .basal.met(cons, mFrame = frameMass, mMusc = muscMass, ordo = taxon)

    # specific work
    specWorkS <- pmech[1]/(muscMass * wingFreq)

    ratioChange <- (cons$efat/cons$eprot)*(1 + cons$hprot)

    mmusc <- 0

    mfat <- 0

    specWork <- vector()
    specWork[1] <- 0
    j <- 1
    while (specWork[j] != specWorkS) {

      # increase muscle mass

      # decrease mass of f

    }
  }


  #rangeL <- rep(list(vector()), length(pmech))
  # rangeSim <- matrix(nrow = length(pmech))
  #
  # fatSim <- matrix(nrow = length(pmech))
  # fatSim[, 1] <- fatFrac
  #
  # pmechSim <- matrix(nrow = length(pmech))
  # pmechSim[, 1] <- pmech
  # specWork <- pmech/(muscMass * wingFreq)
  # bodyMassSim <- matrix(nrow = length(pmech))
  #
  # for (i in 1:nrow(pmechSim)) {
  #   for (j in 1:ncol(pmechSim)) {
  #     if (fatSim[i, j] == 0) {
  #       break()
  #     } else{
  #
  #     }
  #   }
  # }

  # return object of class flysim
  class(results) <- append(class(results), "csw")
  # return class object
  return(results)
}
