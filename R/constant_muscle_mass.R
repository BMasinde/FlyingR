# Constant muscle mass time marching computation
# @author Brain Masinde.
# @name .constant.muscle.mass
# @param data Data as output from .colnames.match
# @param constants
# @param speed_control speed control as either
# @param protein_met percentage of energy attributed to protein

# Rules ########################################################################
# variables are in camelCase except constants abrv
# functions are in function_name
# file name file.name.ext
################################################################################
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

  if (speed_control != 1 && speed_control != 0) {
    stop("speed control should either be 1 or 0")
  }

  # number of observations
  n <- nrow(data)
  allMass <- data$allMass
  fatMass <- data$fatMass
  wingSpan <- data$wingSpan
  wingArea <- data$wingArea
  muscleMass <- data$muscleMass
  taxon <- data$taxon

  # basal metabolic constants
  alphaPasserines <- constants$alpha[1]
  alphaNonPasserines <- constants$alpha[2]
  deltaPasserines <- constants$delta[1]
  deltaNonPasserines <- constants$delta[2]

  # time marching ##############################################################

  results <- list(
    distance = vector(length = n),
    allUpMass = vector(length = n),
    fatMass = vector(length = n),
    muscleMass = vector(length = n),
    startMinSpeed = vector(length = n),
    endMinSpeed = vector(length = n)
  )

  if (speed_control == 1) {

    for (i in seq_len(nrow(data))) {
      # things to keep track of ################################################
      bm <- allMass[i]
      airframeMass <- allMass[i] - (fatMass[i] + muscleMass[i]) # airframe mass
      fm <- fatMass[i]
      dist <- 0
      minSpeed <- vector()
      trueSpeed <- 0

      j <- 1
      while (fm > 0.000001) {
        # find speed and power
        minSpeed[j] <-
          .minpowspeed_cpp(bm = bm, ws = wingSpan[i], ipf = constants$ipf,
                          g = constants$g, airDensity = constants$airDensity, bdc = constants$bdc)
        #  constant speed always
        trueSpeed <- minSpeed[1] * constants$speedRatio
        # mechanical power #####################################################
        mechPower <-
          .total_Mech_Pow_cpp(
            bm = bm,
            ws = wingSpan[i],
            wa = wingArea[i],
            vt = trueSpeed,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )


        chemPower <- constants$vcp * (mechPower + constants$mce * .basal_metabolic_pow(
          airframeMass,
          # doesn't change from previous J iteration
          muscleMass[i],
          # changes from previous J iteration
          taxon[i],
          alphaPasserines,
          alphaNonPasserines,
          deltaPasserines,
          deltaNonPasserines
        )) / constants$mce

          # update body ########################################################
          #NEW
          changeAirframe <-
            ifelse(protein_met == 0, 0, ((chemPower * protein_met) / constants$ped) * 360 * constants$phr)
          # protein has to be hydrated


          airframeMass <- airframeMass - changeAirframe
          changeMass <-
            ifelse(protein_met == 0, (chemPower / constants$fed) * 360, ((chemPower - (chemPower * protein_met)) / constants$fed) * 360)
          fm <- fm - changeMass
          bm <- airframeMass + fm + muscleMass[i]

          # update distance ####################################################
          dist <- dist + trueSpeed * 360

          # increment counter
          j <- j + 1
      }
      # store results ##########################################################
      results$distance[i] <- dist
      results$allUpMass[i] <- bm
      results$fatMass[i] <- fm
      results$muscleMass[i] <- muscleMass[i]
      results$startMinSpeed[i] <- minSpeed[1]
      results$endMinSpeed[i] <- minSpeed[j - 1]
    }

  }else {
    for (i in seq_len(nrow(data))) {
      # things to keep track of ################################################
      bm <- allMass[i]
      airframeMass <- allMass[i] - (fatMass[i] + muscleMass[i]) # airframe mass
      fm <- fatMass[i]
      dist <- 0
      minSpeed <- vector(length = n)
      trueSpeed <- 0

      j <- 1
      while (fm > 0.0001) {
        # find speed ###########################################################
        minSpeed[j] <-
          .minpowspeed_cpp(bm = bm, ws = wingSpan[i], ipf = constants$ipf,
                          g = constants$g, airDensity = constants$airDensity, bdc = constants$bdc)

        # constant trueSpeed to minimum power speed always
        trueSpeed <- minSpeed[j] * constants$speedRatio

        #ratio_speed <- trueSpeed / minSpeed

        # mechanical power #####################################################
        mechPower <-
          .total_Mech_Pow_cpp(bm = bm,
                             ws = wingSpan[i],
                             wa = wingArea[i],
                             vt = trueSpeed,
                             g = constants$g, airDensity = constants$airDensity, ipf = constants$ipf,
                             bdc = constants$bdc, ppc = constants$ppc)

        #if (protein_met == 0) {
          # chemical power
          # E <-
          #   (mechPower / constants$mce) + .basal.met2(constants, bm, fm, taxon[i])
        chemPower <- constants$vcp * (mechPower + constants$mce * .basal_metabolic_pow(
          airframeMass,
          # doesn't change from previous J iteration
          muscleMass[i],
          # changes from previous J iteration
          taxon[i],
          alphaPasserines,
          alphaNonPasserines,
          deltaPasserines,
          deltaNonPasserines
        )) / constants$mce

          # update body ########################################################
          #NEW
          changeAirframe <-
            ifelse(protein_met == 0, 0, ((chemPower * protein_met) / constants$ped) * 360 * constants$phr)
          airframeMass <- airframeMass - changeAirframe
          #changeMass <- (E / constants$fed) * 360
          changeMass <-
            ifelse(protein_met == 0, (chemPower / constants$fed) * 360, ((chemPower - (chemPower * protein_met)) / constants$fed) * 360)
          fm <- fm - changeMass
          bm <- airframeMass + fm + muscleMass[i]

          # distance increment #################################################
          dist <- dist + trueSpeed * 360

          # increment counter
          j <- j + 1
      }
      # store results ##########################################################
      results$distance[i] <- dist
      results$allUpMass[i] <- bm
      results$fatMass[i] <- fm
      results$muscleMass[i] <- muscleMass[i]
      results$startMinSpeed[i] <- minSpeed[1]
      results$endMinSpeed[i] <- minSpeed[j - 1]
    }

  }
  return(results)
}
