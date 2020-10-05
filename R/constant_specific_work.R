# constant specific work time marching computation
# @author Brain Masinde.
# @name .constant.specific.work
# @param data Data as output from .colnames.match
# @param constants
# @param speed_control speed control as either

.constant.specific.work <- function(data, constants, speed_control, protein_met) {

# defensives ###################################################################
  if (missing(data) == TRUE) {
    stop("Missing data argument", call. = FALSE)
  }

  if (missing(constants) == TRUE) {
    stop("Missing constants", call. = FALSE)
  }

  if (missing(speed_control) == TRUE) {
    stop("Missing speed control method", call. = FALSE)
  }

  if (speed_control != 1 && speed_control != 0) {
    stop("speed control should either be 1 or 0")
  }

  # muscle mass is a must for this function
  if (is.null(data$muscleMass)) {
    stop("Muscle mass column missing", call.FALSE = TRUE)
  }
################################################################################
  # number of observations
  n <- nrow(data)

  allMass <- data$allMass

  fatMass <- data$fatMass

  wingSpan <- data$wingSpan

  wingArea <- data$wingArea

  muscleMass <- data$muscleMass

  taxon <- data$taxon

  # get fractions to get frame mass
  fatFraction <- fatMass/allMass

  muscleFraction <- muscleMass/allMass

  #airframeMass <- allMass - (fatMass + muscleMass) # airframe mass

# time marching ################################################################

  # basal metabolic constants
  alphaPasserines <- constants$alpha[1]
  alphaNonPasserines <- constants$alpha[2]
  deltaPasserines <- constants$delta[1]
  deltaNonPasserines <- constants$delta[2]

  results <- list(
    distance = vector(length = n),
    allUpMass = vector(length = n),
    fatMass = vector(length = n),
    muscleMass = vector(length = n),
    startMinSpeed = vector(length = n),
    endMinSpeed = vector(length = n)
  )

  if (speed_control == 1 && protein_met == 0) {
    for (i in seq_len(nrow(data))) {
      # things to keep track of ################################################
      bm <- allMass[i]
      fm <- fatMass[i]
      mm <- muscleMass[i]
      airframeMass <- allMass[i] - (fatMass[i] + muscleMass[i]) # airframe mass
      dist <- 0

      # true start speed
      startMinSpeed <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )

      # we want to hold true speed constant
      trueSpeed <- startMinSpeed * constants$speedRatio

      mechPower <-
        .mechanical_power(
          bm = bm,
          ws = wingSpan[i],
          wa = wingArea[i],
          tas =  trueSpeed,
          g = constants$g,
          airDensity = constants$airDensity,
          ipf = constants$ipf,
          bdc = constants$bdc,
          ppc = constants$ppc
        )

      wingFreq <-
        .wingbeat.freq(bm = bm,
                       ws = wingSpan[i],
                       wa = wingArea[i],
                       constants)
      # subdivide muscle mass into respective components #######################
      myofibrils <-
        muscleMass[i] * (1 - constants$mipd * (mechPower /
                                                 muscleMass[i]) * constants$muscDensity)
      mitochondria <- muscleMass[i] - myofibrils

      # specific work at start of flight #######################################
      specWorkStart <- mechPower / (myofibrils * wingFreq)

      # return starting minimum power speed ####################################
      results$startMinSpeed[i] <- startMinSpeed

      j <- 1
      while (fm > 0.000001) {
        # mechanical power from power curve holding true air-speed constant ####
        mechPower <-
          .mechanical_power(
            bm = bm,
            # changes from previous J iteration
            ws = wingSpan[i],
            wa = wingArea[i],
            tas =  trueSpeed,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )

        # wing frequency
        wingFreq <-
          .wingbeat.freq(bm = bm,
                         # changes from previous J iteration
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # chemical power #######################################################
        chemPower <- (mechPower / constants$mce) +
          .basal_metabolic_pow(
            airframeMass, # doesn't change from previous J iteration
            mm, # changes from previous J iteration
            taxon[i],
            alphaPasserines,
            alphaNonPasserines,
            deltaPasserines,
            deltaNonPasserines
          )

        # increase chemical power by 10% to account for heart and respiratory
        chemPower <- chemPower + (chemPower * 0.1)

        # fat consumed in the interval?
        usedFat <- chemPower / constants$fed * 360

        #Reduce body composition by this consumed fat and determine what amount
        # of protein to consume to achieve specific power at start of flight
        bmDummy <- bm - usedFat
        fmDummy <- fm - usedFat

        # because of this reduction minimum speed, mechanical power, and
        # wing frequency reduce
        trueSpeedDummy <-
          .minpowspeed_cpp(
            bm = bmDummy,
            ws = wingSpan[i],
            ipf = constants$ipf,
            g = constants$g,
            airDensity = constants$airDensity,
            bdc = constants$bdc
          ) * constants$speedRatio

        mechPowerDummy <-
          .mechanical_power(
            bm = bmDummy,
            ws = wingSpan[i],
            wa = wingArea[i],
            tas = trueSpeedDummy,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )

        # wing frequency
        wingFreqDummy <-
          .wingbeat.freq(bm = bmDummy,
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # amount of protein consumed that restores specific work to initial value
        usedProtein <-
          -(mechPowerDummy / (wingFreqDummy * specWorkStart)) + myofibrils

        # amount of fuel energy released is found by multiplying the mass of dry protein
        # removed by the energy density of dry protein
        usedFatEquiv <-
          (usedProtein * constants$ped) / constants$fed

        # update body measurements #############################################
        fm <- fm - (usedFat - usedFatEquiv)
        #cat("current fat mass = ", fm, sep = " ", "\n")
        # adjustment of needed for myofibrils?
        myofibrils <- myofibrils - (usedProtein * constants$phr)
        mm <- mitochondria + myofibrils

        # have to subtract the fat equivalent and water lost in the process
        bm <- bm - (usedFat - usedFatEquiv) - (usedProtein * constants$phr)

        # distance increment ###################################################
        dist <- dist + trueSpeed * 360

        # increase counter
        j <-  j + 1
        #cat("current iteration", j, sep = " ", "\n")
      }
      results$distance[i] <- dist
      results$allUpMass[i] <- bm
      results$fatMass[i] <- fm
      results$muscleMass[i] <- mm
      results$endMinSpeed[i] <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )
    }
  }  else if (speed_control == 1 && protein_met > 0) {
    for (i in seq_len(nrow(data))) {
      # things to keep track of ################################################
      bm <- allMass[i]
      fm <- fatMass[i]
      mm <- muscleMass[i]
      airframeMass <- allMass[i] - (fatMass[i] + muscleMass[i])
      dist <- 0

      # true start speed
      startMinSpeed <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )

      # we want to hold true speed constant
      trueSpeed <- startMinSpeed * constants$speedRatio

      mechPower <-
        .mechanical_power(
          bm = bm,
          ws = wingSpan[i],
          wa = wingArea[i],
          tas =  trueSpeed,
          g = constants$g,
          airDensity = constants$airDensity,
          ipf = constants$ipf,
          bdc = constants$bdc,
          ppc = constants$ppc
        )

      wingFreq <-
        .wingbeat.freq(bm = bm,
                       ws = wingSpan[i],
                       wa = wingArea[i],
                       constants)
      # subdivide muscle mass into respective components #######################
      myofibrils <-
        muscleMass[i] * (1 - constants$mipd * (mechPower /
                                                 muscleMass[i]) * constants$muscDensity)
      mitochondria <- muscleMass[i] - myofibrils

      # specific work at start of flight #######################################
      specWorkStart <- mechPower / (myofibrils * wingFreq)

      # return starting minimum power speed ####################################
      results$startMinSpeed[i] <- startMinSpeed

      j <- 1
      while (fm > 0.000001) {
        # mechanical power from power curve holding true air-speed constant
        mechPower <-
          .mechanical_power(
            bm = bm,
            ws = wingSpan[i],
            wa = wingArea[i],
            tas =  trueSpeed,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )
        #cat("mech power within while", mechPower, sep = " ", "\n")
        # wing frequency
        wingFreq <-
          .wingbeat.freq(bm = bm,
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # chemical power #######################################################
        chemPower <- (mechPower / constants$mce) +
          .basal_metabolic_pow(
            airframeMass, # doesn't change from previous J iteration
            mm, # changes from previous J iteration
            taxon[i],
            alphaPasserines,
            alphaNonPasserines,
            deltaPasserines,
            deltaNonPasserines
          )

        # increase chemPower by 10% respiratory
        chemPower <- chemPower + (chemPower * 0.1)

        #cat("chem power within while", chemPower, sep = " ", "\n")

        # energy that should be attributed to protein
        EFromProtein <- chemPower * protein_met

        # fat consumed in the interval?
        #usedFat <- ((chemPower - EFromProtein)  / constants$fed) * 360
        usedFat <- (chemPower  / constants$fed) * 360
        #Reduce body composition by this consumed fat and determine what amount
        # of protein to consume to achieve specific power at start of flight
        bmDummy <- bm - usedFat


        #cat("expected change in bm", bmDummy, sep = " ", "\n")
        # because of this reduction minimum speed, mechanical power, and
        # wing frequency reduce

        trueSpeedDummy <-
          .minpowspeed_cpp(
            bm = bmDummy,
            ws = wingSpan[i],
            ipf = constants$ipf,
            g = constants$g,
            airDensity = constants$airDensity,
            bdc = constants$bdc
          ) * constants$speedRatio

        mechPowerDummy <-
          .mechanical_power(
            bm = bmDummy,
            ws = wingSpan[i],
            wa = wingArea[i],
            tas = trueSpeedDummy,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )

        wingFreqDummy <-
          .wingbeat.freq(bm = bmDummy,
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # amount of protein consumed that restores specific work to initial value
        usedProtein <-
          -(mechPowerDummy / (wingFreqDummy * specWorkStart)) + myofibrils

        # amount of fuel energy released is found by multiplying the mass of dry protein
        # removed by the energy density of dry protein
        usedFatEquiv <-
          (usedProtein * constants$ped) / constants$fed

        # adjust body components ###############################################
        fm <- fm - (usedFat - usedFatEquiv)

        myofibrils <- myofibrils - (usedProtein * constants$phr)
        mm <- mitochondria + myofibrils
        airframeMass <- airframeMass - ((EFromProtein/constants$ped) * 360 * constants$phr)
        #cat("airframe mass", airframeMass, sep = " ", "\n")
        #bm <- bm - (usedFat - usedFatEquiv) - (usedProtein * constants$phr) - (EFromProtein/constants$ped)

        bm <- airframeMass + mm + fm

        # distance increment ###################################################
        dist <- dist + trueSpeed * 360
        #cat("distance", dist, sep = " ", "\n")

        # increase counter
        j <-  j + 1
        #cat("iteration", j, sep = " ", "\n")
      }
      results$distance[i] <- dist
      results$allUpMass[i] <- bm
      results$fatMass[i] <- fm
      results$muscleMass[i] <- mm
      results$endMinSpeed[i] <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )
    } # closes for loop
  } else if (speed_control == 0 && protein_met == 0) {
    for (i in seq_len(nrow(data))) {
      # things to keep track of ################################################
      bm <- allMass[i]
      fm <- fatMass[i]
      mm <- muscleMass[i]
      airframeMass <- allMass[i] - (fatMass[i] + muscleMass[i]) # airframe mass
      dist <- 0

      # true start speed
      startMinSpeed <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )

      # we want to hold ratio of minimum power speed and true speed constant
      trueSpeed <- startMinSpeed * constants$speedRatio

      mechPower <-
        .mechanical_power(
          bm = bm,
          ws = wingSpan[i],
          wa = wingArea[i],
          tas =  trueSpeed,
          g = constants$g,
          airDensity = constants$airDensity,
          ipf = constants$ipf,
          bdc = constants$bdc,
          ppc = constants$ppc
        )

      wingFreq <-
        .wingbeat.freq(bm = bm,
                       ws = wingSpan[i],
                       wa = wingArea[i],
                       constants)
      # subdivide muscle mass into respective components #######################
      myofibrils <-
        muscleMass[i] * (1 - constants$mipd * (mechPower /
                                                 muscleMass[i]) * constants$muscDensity)
      mitochondria <- muscleMass[i] - myofibrils

      # specific work at start of flight #######################################
      specWorkStart <- mechPower / (myofibrils * wingFreq)

      # return starting minimum power speed ####################################
      results$startMinSpeed[i] <- startMinSpeed

      j <- 1
      while (fm > 0.000001) {
        # hold ratio of minimum power speed and true airspeed constant
        # true start speed
        minSpeed <- .minpowspeed_cpp(
          bm = bm, # bm changes after each J iteration
          ws = wingSpan[i],
          ipf = constants$ipf,
          g = constants$g,
          airDensity = constants$airDensity,
          bdc = constants$bdc
        )

        # we want to hold ratio of minimum power speed and true speed constant
        trueSpeed <- minSpeed * constants$speedRatio

        # mechanical power from power curve holding true air-speed constant ####
        mechPower <-
          .mechanical_power(
            bm = bm,
            # changes from previous J iteration
            ws = wingSpan[i],
            wa = wingArea[i],
            tas =  trueSpeed,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )

        # wing frequency
        wingFreq <-
          .wingbeat.freq(bm = bm,
                         # changes from previous J iteration
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # chemical power #######################################################
        chemPower <- (mechPower / constants$mce) +
          .basal_metabolic_pow(
            airframeMass,
            # doesn't change from previous J iteration
            mm,
            # changes from previous J iteration
            taxon[i],
            alphaPasserines,
            alphaNonPasserines,
            deltaPasserines,
            deltaNonPasserines
          )

        # increase chemical power by 10% to account for heart and respiratory
        chemPower <- chemPower + (chemPower * 0.1)

        # fat consumed in the interval?
        usedFat <- chemPower / constants$fed * 360

        #Reduce body composition by this consumed fat and determine what amount
        # of protein to consume to achieve specific power at start of flight
        bmDummy <- bm - usedFat
        fmDummy <- fm - usedFat

        # because of this reduction minimum speed, mechanical power, and
        # wing frequency reduce
        trueSpeedDummy <-
          .minpowspeed_cpp(
            bm = bmDummy,
            ws = wingSpan[i],
            ipf = constants$ipf,
            g = constants$g,
            airDensity = constants$airDensity,
            bdc = constants$bdc
          ) * constants$speedRatio

        mechPowerDummy <-
          .mechanical_power(
            bm = bmDummy,
            ws = wingSpan[i],
            wa = wingArea[i],
            tas = trueSpeedDummy,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )

        # wing frequency
        wingFreqDummy <-
          .wingbeat.freq(bm = bmDummy,
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # amount of protein consumed that restores specific work to initial value
        usedProtein <-
          -(mechPowerDummy / (wingFreqDummy * specWorkStart)) + myofibrils

        # amount of fuel energy released is found by multiplying the mass of dry protein
        # removed by the energy density of dry protein
        usedFatEquiv <-
          (usedProtein * constants$ped) / constants$fed

        # update body measurements #############################################
        fm <- fm - (usedFat - usedFatEquiv)

        # adjustment of needed for myofibrils?
        #myofibrils <- myofibrils - (usedProtein * constants$phr)
        mm <- mitochondria + myofibrils

        bm <- bm - (usedFat - usedFatEquiv)

        # distance increment ###################################################
        dist <- dist + trueSpeed * 360

        # increase counter
        j <-  j + 1
      }
      results$distance[i] <- dist
      results$allUpMass[i] <- bm
      results$fatMass[i] <- fm
      results$muscleMass[i] <- mm
      results$endMinSpeed[i] <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )
    } # closes for loop (iterates over rows of species)
  } else if (speed_control == 0 && protein_met > 0) {
    for (i in seq_len(nrow(data))) {
      # things to keep track of ################################################
      bm <- allMass[i]
      fm <- fatMass[i]
      mm <- muscleMass[i]
      airframeMass <- allMass[i] - (fatMass[i] + muscleMass[i]) # airframe mass
      dist <- 0

      # true start speed
      startMinSpeed <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )

      # we want to hold true speed constant
      trueSpeed <- startMinSpeed * constants$speedRatio

      mechPower <-
        .mechanical_power(
          bm = bm,
          ws = wingSpan[i],
          wa = wingArea[i],
          tas =  trueSpeed,
          g = constants$g,
          airDensity = constants$airDensity,
          ipf = constants$ipf,
          bdc = constants$bdc,
          ppc = constants$ppc
        )

      wingFreq <-
        .wingbeat.freq(bm = bm,
                       ws = wingSpan[i],
                       wa = wingArea[i],
                       constants)
      # subdivide muscle mass into respective components #######################
      myofibrils <-
        muscleMass[i] * (1 - constants$mipd * (mechPower /
                                                 muscleMass[i]) * constants$muscDensity)
      mitochondria <- muscleMass[i] - myofibrils

      # specific work at start of flight #######################################
      specWorkStart <- mechPower / (myofibrils * wingFreq)

      # return starting minimum power speed ####################################
      results$startMinSpeed[i] <- startMinSpeed

      j <- 1
      while (fm > 0.000001) {
        # mechanical power from power curve holding true air-speed constant
        mechPower <-
          .mechanical_power(
            bm = bm,
            ws = wingSpan[i],
            wa = wingArea[i],
            tas =  trueSpeed,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )

        # wing frequency
        wingFreq <-
          .wingbeat.freq(bm = bm,
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # chemical power #######################################################
        chemPower <- (mechPower / constants$mce) +
          .basal_metabolic_pow(
            airframeMass = airframeMass, # doesn't change from previous J iteration
            muscleMass = mm, # changes from previous J iteration
            taxon = taxon[i],
            alphaPasserines = alphaPasserines,
            alphaNonPasserines = alphaNonPasserines,
            deltaPasserines = deltaPasserines,
            deltaNonPasserines = deltaNonPasserines
          )

        # increase chemPower by 10% respiratory
        chemPower <- chemPower + (chemPower * 0.1)

        # energy that should be attributed to protein
        EFromProtein <- chemPower * protein_met

        # fat consumed in the interval?
        usedFat <- (chemPower - EFromProtein)  / constants$fed * 360

        #Reduce body composition by this consumed fat and determine what amount
        # of protein to consume to achieve specific power at start of flight
        bmDummy <- bm - usedFat
        fmDummy <- fm - usedFat

        # because of this reduction minimum speed, mechanical power, and
        # wing frequency reduce

        trueSpeedDummy <-
          .minpowspeed_cpp(
            bm = bmDummy,
            ws = wingSpan[i],
            ipf = constants$ipf,
            g = constants$g,
            airDensity = constants$airDensity,
            bdc = constants$bdc
          ) * constants$speedRatio

        mechPowerDummy <-
          .mechanical_power(
            bm = bmDummy,
            ws = wingSpan[i],
            wa = wingArea[i],
            tas = trueSpeedDummy,
            g = constants$g,
            airDensity = constants$airDensity,
            ipf = constants$ipf,
            bdc = constants$bdc,
            ppc = constants$ppc
          )

        wingFreqDummy <-
          .wingbeat.freq(bm = bmDummy,
                         ws = wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # amount of protein consumed that restores specific work to initial value
        usedProtein <-
          -(mechPowerDummy / (wingFreqDummy * specWorkStart)) + myofibrils

        # amount of fuel energy released is found by multiplying the mass of dry protein
        # removed by the energy density of dry protein
        usedFatEquiv <- (usedProtein * constants$ped) / constants$fed

        # adjust body components ###############################################
        fm <- fm - (usedFat - usedFatEquiv)
        myofibrils <- myofibrils - (usedProtein - constants$phr)
        mm <- mitochondria + myofibrils
        bm <- airframeMass + mm + fm

        # distance increment ###################################################
        dist <- dist + trueSpeed * 360
        # increase counter
        j <-  j + 1
      }
      results$distance[i] <- dist
      results$allUpMass[i] <- bm
      results$fatMass[i] <- fm
      results$muscleMass[i] <- mm
      results$endMinSpeed[i] <- .minpowspeed_cpp(
        bm = bm,
        ws = wingSpan[i],
        ipf = constants$ipf,
        g = constants$g,
        airDensity = constants$airDensity,
        bdc = constants$bdc
      )
    }
  } # closes conditioning speed_control and protein_met
  return(results)
}
