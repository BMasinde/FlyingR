#' Constant muscle mass time marching computation
#' @author Brain Masinde.
#' @name constant_muscle_mass
#' @param data Data as output from .colnames.match
#' @param cons
#' @param speed_control speed control as either


.constant_muscle_mass <- function(data, cons, speed_control) {
  if (missing(data) == TRUE) {
    stop("Missing data argument")
  }

  if(missing(cons) == TRUE){
    stop("Missing constants")
  }

  if(missing(speed_control) == TRUE){
    stop("Missing speed control method")
  }

  allMass <- data$allMass

  fatMass <- data$fatMass

  wingSpan <- data$wingSpan

  wingArea <- data$wingArea

  id <- data$id

  name <- data$name

  taxon <- data$taxon

  # time marching

  simResults <- list(
    bm = rep(list(vector()), nrow(data)),
    fm = rep(list(vector()), nrow(data)),
    dist = rep(list(vector()), nrow(data)),
    deltaM = rep(list(vector()), nrow(data)),
    mechPow = rep(list(vector()), nrow(data)),
    E = rep(list(vector()), nrow(data)),
    minSpeed = rep(list(vector()), nrow(data)),
    trueSpeed = rep(list(vector()), nrow(data)),
    rvvmp = rep(list(vector()), nrow(data))
  )

  if (speed_control == "constant_speed") {

    for (i in 1:nrow(data)) {
      #i <- 1
      # initial values
      simResults$bm[[i]][1] <- allMass[i]
      simResults$fm[[i]][1] <- fatMass[i]
      currentFM <- simResults$fm[[i]][1]

      j <- 1
      while (currentFM > 0.000001) {
        # find speed and power
        simResults$minSpeed[[i]][j] <-
          .minpowspeed_cpp(bm = simResults$bm[[i]][j], ws = wingSpan[i], ipf = cons$ipf,
                          g = cons$g, airDensity = cons$airDensity, bdc = cons$bdc)
        #  constant speed always
        simResults$trueSpeed[[i]][j] <- simResults$minSpeed[[i]][1] * cons$speedRatio

        simResults$rvvmp[[i]][j] <-
          simResults$trueSpeed[[i]][j] / simResults$minSpeed[[i]][j]

        # mechanical power
        power <-
          .total_Mech_Pow_cpp(bm =simResults$bm[[i]][j],
                            ws = wingSpan[i],
                            wa = wingArea[i],
                            vt = simResults$trueSpeed[[i]][j],
                            g = cons$g, airDensity = cons$airDensity, ipf = cons$ipf,
                            bdc = cons$bdc, ppc = cons$ppc)
        simResults$mechPow[[i]][j] <- power

        # chemical power
        E <-
          (power / cons$mce) + .basal.met2(cons, simResults$bm[[i]][j], simResults$fm[[i]][j], taxon[i])

        #increase E by 10% to account for respiratory and heart
        E <- E + (E * 0.1)

        simResults$E[[i]][j] <- E

        # update body
        simResults$deltaM[[i]][j] <- (simResults$E[[i]][j] / cons$eFat) * 360
        simResults$fm[[i]][j + 1] <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
        simResults$bm[[i]][j + 1] <- simResults$bm[[i]][j] - simResults$deltaM[[i]][j]

        currentFM <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
        simResults$dist[[i]][j] <- simResults$trueSpeed[[i]][j] * 360
        # increment counter
        j <- j + 1
      }
    }

  }else {
    for (i in 1:nrow(data)) {
      #i <- 1
      # initial values
      simResults$bm[[i]][1] <- allMass[i]
      simResults$fm[[i]][1] <- fatMass[i]
      currentFM <- simResults$fm[[i]][1]

      j <- 1
      while (currentFM > 0.000001) {
        # find speed and power
        simResults$minSpeed[[i]][j] <-
          .minpowspeed_cpp(bm = simResults$bm[[i]][j], ws = wingSpan[i], ipf = cons$ipf,
                          g = cons$g, airDensity = cons$airDensity, bdc = cons$bdc)
        # constant true-speed to minimum power speed always
        simResults$trueSpeed[[i]][j] <- simResults$minSpeed[[i]][j] * cons$speedRatio

        simResults$rvvmp[[i]][j] <-
          simResults$trueSpeed[[i]][j] / simResults$minSpeed[[i]][j]

        # mechanical power
        power <-
          .total_Mech_Pow_cpp(bm =simResults$bm[[i]][j],
                             ws = wingSpan[i],
                             wa = wingArea[i],
                             vt = simResults$trueSpeed[[i]][j],
                             g = cons$g, airDensity = cons$airDensity, ipf = cons$ipf,
                             bdc = cons$bdc, ppc = cons$ppc)
        simResults$mechPow[[i]][j] <- power

        # chemical power
        E <-
          (power / cons$mce) + .basal.met2(cons, simResults$bm[[i]][j], simResults$fm[[i]][j], taxon[i])

        #increase E by 10% to account for respiratory and heart
        E <- E + (E * 0.1)

        simResults$E[[i]][j] <- E

        # update body
        simResults$deltaM[[i]][j] <- (simResults$E[[i]][j] / cons$eFat) * 360
        simResults$fm[[i]][j + 1] <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
        simResults$bm[[i]][j + 1] <- simResults$bm[[i]][j] - simResults$deltaM[[i]][j]

        currentFM <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
        simResults$dist[[i]][j] <- simResults$trueSpeed[[i]][j] * 360
        # increment counter
        j <- j + 1
      }
    }
  }

  return(simResults)
}
