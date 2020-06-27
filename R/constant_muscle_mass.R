# Constant muscle mass time marching computation
# @author Brain Masinde.
# @name .constant.muscle.mass
# @param data Data as output from .colnames.match
# @param constants
# @param speed_control speed control as either
# @param protein_met percentage of energy attributed to protein


.constant.muscle.mass <- function(data, constants, speed_control, protein_met) {
  if (missing(data) == TRUE) {
    stop("Missing data argument")
  }

  if (missing(constants) == TRUE) {
    stop("Missing constants")
  }

  if (missing(speed_control) == TRUE) {
    stop("Missing speed control method")
  }

  allMass <- data$allMass

  fatMass <- data$fatMass

  wingSpan <- data$wingSpan

  wingArea <- data$wingArea

  muscleMass <- data$muscleMass

  id <- data$id

  name <- data$name

  taxon <- data$taxon

  fatFrac <- fatMass / allMass
  muscleFrac <- muscleMass / allMass

  # time marching

  simResults <- list(
    bm = rep(list(vector()), nrow(data)),
    afm = rep(list(vector()), nrow(data)), # airframe mass
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

    for (i in seq_len(nrow(data))) {
      # initial values
      simResults$bm[[i]][1] <- allMass[i]
      simResults$fm[[i]][1] <- fatMass[i]
      simResults$afm[[i]][1] <- allMass[i] - (fatMass[i] + muscleMass[i])
      currentFM <- simResults$fm[[i]][1]

      j <- 1
      while (currentFM > 0.000001) {
        # find speed and power
        simResults$minSpeed[[i]][j] <-
          .minpowspeed_cpp(bm = simResults$bm[[i]][j], ws = wingSpan[i], ipf = constants$inducedPowerFactor,
                          g = constants$g, airDensity = constants$airDensity, bdc = constants$bodyDragCoef)
        #  constant speed always
        simResults$trueSpeed[[i]][j] <- simResults$minSpeed[[i]][1] * constants$speedRatio

        simResults$rvvmp[[i]][j] <-
          simResults$trueSpeed[[i]][j] / simResults$minSpeed[[i]][j]

        # mechanical power
        power <-
          .total_Mech_Pow_cpp(
            bm = simResults$bm[[i]][j],
            ws = wingSpan[i],
            wa = wingArea[i],
            vt = simResults$trueSpeed[[i]][j],
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$inducedPowerFactor,
            bdc = constants$bodyDragCoef,
            ppc = constants$profPowerConstant
          )
        simResults$mechPow[[i]][j] <- power

        if (protein_met == 0) {
          # chemical power
          E <-
            (power / constants$efficiency) + .basal.met2(constants, simResults$bm[[i]][j], simResults$fm[[i]][j], taxon[i])

          # increase E by 10% to account for respiratory and heart
          E <- E + (E * 0.1)

          simResults$E[[i]][j] <- E

          # update body
          simResults$deltaM[[i]][j] <-
            (simResults$E[[i]][j] / constants$fatEnergy) * 360
          simResults$fm[[i]][j + 1] <-
            simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$bm[[i]][j + 1] <-
            simResults$bm[[i]][j] - simResults$deltaM[[i]][j]

          currentFM <-
            simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$dist[[i]][j] <-
            simResults$trueSpeed[[i]][j] * 360
          # increment counter
          j <- j + 1

        } else {
          # chemical power
          E <-
            (power / constants$efficiency) + .basal.met2(constants, simResults$bm[[i]][j], simResults$fm[[i]][j], taxon[i])

          # increase E by 10% to account for respiratory and heart
          E <- E + (E * 0.1)

          # total energy
          simResults$E[[i]][j] <- E

          # protein used during metabolism
          EFromProtein <- E * protein_met

          # convert this energy to mass by dividing by enegry content of protein
          deltaAFM <- EFromProtein / constants$proteinEnergy

          # update body
          # deltaM is the change in mass (subtracting E attributed to protein)
          simResults$deltaM[[i]][j] <- ((E - EFromProtein) / constants$fatEnergy) * 360
          simResults$afm[[i]][j + 1] <- simResults$afm[[i]][j] - deltaAFM
          simResults$fm[[i]][j + 1] <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$bm[[i]][j + 1] <-
            simResults$afm[[i]][j + 1] + simResults$fm[[i]][j + 1] + muscleMass[i]

          currentFM <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$dist[[i]][j] <- simResults$trueSpeed[[i]][j] * 360
          # increment counter
          j <- j + 1
        }
      }
    }

  }else {
    for (i in seq_len(nrow(data))) {
      #i <- 1
      # initial values
      simResults$bm[[i]][1] <- allMass[i]
      simResults$fm[[i]][1] <- fatMass[i]
      simResults$afm[[i]][1] <- allMass[i] - (fatMass[i] + muscleMass[i])
      currentFM <- simResults$fm[[i]][1]

      j <- 1
      while (currentFM > 0.000001) {
        # find speed and power
        simResults$minSpeed[[i]][j] <-
          .minpowspeed_cpp(bm = simResults$bm[[i]][j], ws = wingSpan[i], ipf = constants$inducedPowerFactor,
                          g = constants$g, airDensity = constants$airDensity, bdc = constants$bodyDragCoef)
        # constant true-speed to minimum power speed always
        simResults$trueSpeed[[i]][j] <- simResults$minSpeed[[i]][j] * constants$speedRatio

        simResults$rvvmp[[i]][j] <-
          simResults$trueSpeed[[i]][j] / simResults$minSpeed[[i]][j]

        # mechanical power
        power <-
          .total_Mech_Pow_cpp(bm = simResults$bm[[i]][j],
                             ws = wingSpan[i],
                             wa = wingArea[i],
                             vt = simResults$trueSpeed[[i]][j],
                             g = constants$g, airDensity = constants$airDensity, ipf = constants$inducedPowerFactor,
                             bdc = constants$bodyDragCoef, ppc = constants$profPowerConstant)
        simResults$mechPow[[i]][j] <- power

        if (protein_met == 0) {
          # chemical power
          E <-
            (power / constants$efficiency) + .basal.met2(constants, simResults$bm[[i]][j], simResults$fm[[i]][j], taxon[i])

          #increase E by 10% to account for respiratory and heart
          E <- E + (E * 0.1)

          simResults$E[[i]][j] <- E

          # update body
          simResults$deltaM[[i]][j] <-
            (simResults$E[[i]][j] / constants$fatEnergy) * 360
          simResults$fm[[i]][j + 1] <-
            simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$bm[[i]][j + 1] <-
            simResults$bm[[i]][j] - simResults$deltaM[[i]][j]

          currentFM <-
            simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$dist[[i]][j] <-
            simResults$trueSpeed[[i]][j] * 360
          # increment counter
          j <- j + 1
        } else {
          # chemical power
          E <-
            (power / constants$efficiency) + .basal.met2(constants, simResults$bm[[i]][j], simResults$fm[[i]][j], taxon[i])

          #increase E by 10% to account for respiratory and heart
          E <- E + (E * 0.1)

          simResults$E[[i]][j] <- E
          # protein used during metabolism
          EFromProtein <- E * protein_met

          # convert this energy to mass by dividing by enegry content of protein
          deltaAFM <- EFromProtein / constants$proteinEnergy

          # update body
          simResults$deltaM[[i]][j] <- ((E - EFromProtein) / constants$fatEnergy) * 360
          simResults$afm[[i]][j + 1] <- simResults$afm[[i]][j] - deltaAFM
          simResults$fm[[i]][j + 1] <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$bm[[i]][j + 1] <-
            simResults$afm[[i]][j + 1] + simResults$fm[[i]][j + 1] + muscleMass[i]

          currentFM <- simResults$fm[[i]][j] - simResults$deltaM[[i]][j]
          simResults$dist[[i]][j] <- simResults$trueSpeed[[i]][j] * 360
          # increment counter
          j <- j + 1
        }
      }
    }
  }

  return(simResults)
}
