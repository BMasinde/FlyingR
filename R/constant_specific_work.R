# constant specific work time marching computation
# @author Brain Masinde.
# @name .constant.specific.work
# @param data Data as output from .colnames.match
# @param constants
# @param speed_control speed control as either

.constant.specific.work <- function(data, constants, speed_control, protein_met) {
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

  # number of observations
  n <- nrow(ddata

  allMass <- data$allMass

  fatMass <- data$fatMass

  wingSpan <- data$wingSpan

  wingArea <- data$wingArea

  muscleMass <- data$muscleMass

  taxon <- data$taxon

  # get fractions to get frame mass
  fatFraction <- fatMass/allMass

  muscleFraction <- muscleMass/allMass

  frame_mass <- allMass*(1 - fatFraction - muscleFraction)

  # time marching

  sim_results <- list(
    bm = rep(list(vector()), nrow(data)),
    fm = rep(list(vector()), nrow(data)),
    mm = rep(list(vector()), nrow(data)),
    afm = rep(list(vector()), nrow(data)),
    mitochondria = rep(list(vector()), nrow(data)),
    myofibrils = rep(list(vector()), nrow(data)),
    dist = rep(list(vector()), nrow(data)),
    #delta_m = rep(list(vector()), nrow(data)),
    mechPow = rep(list(vector()), nrow(data)),
    chemPow = rep(list(vector()), nrow(data)),
    min_speed = rep(list(vector()), nrow(data)),
    true_speed = rep(list(vector()), nrow(data)),
    speed_ratio = rep(list(vector()), nrow(data)),
    wing_freq = rep(list(vector()), nrow(data)),
    spec_work = rep(list(vector()), nrow(data))
  )

  if (speed_control == 1) {
    for (i in seq_len(nrow(data))) {
      #i = 2
      sim_results$bm[[i]][1] <- allMass[i]
      sim_results$fm[[i]][1] <- fatMass[i]
      sim_results$mm[[i]][1] <-  muscleMass[i]
      sim_results$afm[[i]][1] <- allMass[i] - (fatMass[i] + muscleMass[i])
      current_fm <- sim_results$fm[[i]][1]

      j <- 1
      while (sim_results$fm[[i]][j] > 0.000001) {
        # calculate speed
        sim_results$min_speed[[i]][j] <-
          .minpowspeed_cpp(
            bm = sim_results$bm[[i]][j],
            ws =wingSpan[i],
            ipf = constants$ipf,
            g = constants$g,
            airDensity = constants$airDensity,
            bdc = constants$bdf
          )
        # true speed (constant speed always)
        sim_results$true_speed[[i]][j] <- sim_results$min_speed[[i]][1] * constants$speedRatio

        # mechanical power from power curve holding true air-speed constant
        sim_results$mechPow[[i]][j] <-
          .pow.curve(bm = sim_results$bm[[i]][j],
                   ws =wingSpan[i],
                   wa = wingArea[i],
                   tas =  sim_results$true_speed[[i]][j], constants = constants)

        # wing frequency
        sim_results$wing_freq[[i]][j] <-
          .wingbeat.freq(bm = sim_results$bm[[i]][j],
                         ws =wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # subdivide muscle mass into mitochondria and myofibrils
        sim_results$myofibrils[[i]][1] <-
          sim_results$mm[[i]][1] * (1 - constants$mipd * (sim_results$mechPow[[i]][1] /
                                                           sim_results$mm[[i]][1]) * constants$muscDensity)
        sim_results$mitochondria[[i]][j] <-
          sim_results$mm[[i]][1] - sim_results$myofibrils[[i]][1]

        # specific power at begining of flight (before fat has been consumed)
        specwork_start <-
          sim_results$mechPow[[i]][1] / (sim_results$myofibrils[[i]][1] * sim_results$wing_freq[[i]][1])

        # chemical power
        deltaE <- (sim_results$mechPow[[i]][j] / constants$mce) +
          .basal.met(
            constants = constants,
            mFrame = frame_mass[i],
            mMusc = sim_results$mm[[i]][j],
            ordo = taxon[i]
          )

        # increase deltaE by 10% respiratory
        sim_results$chemPow[[i]][j] <- deltaE + (deltaE * 0.1)

        if (protein_met == 0) {
          # fat consumed in the interval?
          used_fat <- sim_results$chemPow[[i]][j] / constants$fed * 360

          #Reduce body composition by this consumed fat and determine what amount
          # of protein to consume to achieve specific power at start of flight
          dummy_bm <- sim_results$bm[[i]][j] - used_fat
          dummy_fm <- sim_results$fm[[i]][j] - used_fat

          # because of this reduction minimum speed, mechanical power, and
          # wing frequency reduce

          dummy_true_speed <-
            .minpowspeed_cpp(
              bm = dummy_bm,
              ws =wingSpan[i],
              ipf = constants$ipf,
              g = constants$g,
              airDensity = constants$airDensity,
              bdc = constants$bdf
            ) * constants$speedRatio

          dummy_mechPow <- .pow.curve(bm = dummy_bm, ws =wingSpan[i],
                                      wa = wingArea[i], tas = dummy_true_speed, constants = constants)

          # wing frequency
          dummy_wing_freq <-
            .wingbeat.freq(bm = dummy_bm, ws =wingSpan[i], wa = wingArea[i], constants)

          # specific work after reduction of fat
          dummy_specwork <- dummy_mechPow / (sim_results$myofibrils[[i]][j] * dummy_wing_freq)

          # amount of protein consumed that restores specific work to initial value
          used_protein <-
            -(dummy_mechPow / (dummy_wing_freq * specwork_start)) + sim_results$myofibrils[[i]][j]

          # checking if specific work has been restored
          sim_results$spec_work[[i]][j] <-
            dummy_mechPow / ((sim_results$myofibrils[[i]][j] - used_protein) * dummy_wing_freq)

          # amount of fule energy released is found by multiplying the mass of dry protein
          # removed by the energy density of dry protein
          used_fat_equiv <- (used_protein * constants$ped) / constants$fed

          # adjust body components
          # new fat mass
          sim_results$fm[[i]][j+1] <-  sim_results$fm[[i]][j] - (used_fat - used_fat_equiv)
          sim_results$myofibrils[[i]][j+1] <- sim_results$myofibrils[[i]][j] - (used_protein * constants$phr)
          sim_results$mm[[i]][j+1] <- sim_results$mitochondria[[i]][1] + (sim_results$myofibrils[[i]][j] - (used_protein *constants$phr))
          sim_results$bm[[i]][j+1] <- sim_results$bm[[i]][j] - (used_fat - used_fat_equiv) - (used_protein *constants$phr)
          sim_results$dist[[i]][j] <- sim_results$true_speed[[i]][j] * 360
          j <-  j + 1
        } else {
          # protein requried for metabolic pathways
          EFromProtein <- sim_results$chemPow[[i]][j] * protein_met

          # fat consumed in the interval?
          used_fat <- (sim_results$chemPow[[i]][j] - EFromProtein)  / constants$fed * 360

          #Reduce body composition by this consumed fat and determine what amount
          # of protein to consume to achieve specific power at start of flight
          dummy_fm <- sim_results$fm[[i]][j] - used_fat
          dummy_bm <-
            (sim_results$afm[[i]][j] - (EFromProtein / constants$ped)) + dummy_fm + sim_results$mm[[i]][j]

          dummy_true_speed <-
            .minpowspeed_cpp(
              bm = dummy_bm,
              ws =wingSpan[i],
              ipf = constants$ipf,
              g = constants$g,
              airDensity = constants$airDensity,
              bdc = constants$bdf
            ) * constants$speedRatio

          dummy_mechPow <- .pow.curve(bm = dummy_bm, ws =wingSpan[i],
                                      wa = wingArea[i], tas = dummy_true_speed, constants = constants)

          # wing frequency
          dummy_wing_freq <-
            .wingbeat.freq(bm = dummy_bm, ws =wingSpan[i], wa = wingArea[i], constants)

          # specific work after reduction of fat
          dummy_specwork <- dummy_mechPow / (sim_results$myofibrils[[i]][j] * dummy_wing_freq)

          # amount of protein consumed that restores specific work to initial value
          used_protein <-
            -(dummy_mechPow / (dummy_wing_freq * specwork_start)) + sim_results$myofibrils[[i]][j]

          # checking if specific work has been restored
          sim_results$spec_work[[i]][j] <-
            dummy_mechPow / ((sim_results$myofibrils[[i]][j] - used_protein) * dummy_wing_freq)

          # amount of fule energy released is found by multiplying the mass of dry protein
          # removed by the energy density of dry protein
          used_fat_equiv <- (used_protein * constants$ped) / constants$fed

          # adjust body components
          # new fat mass
          sim_results$fm[[i]][j+1] <-  sim_results$fm[[i]][j] - (used_fat - used_fat_equiv)
          sim_results$myofibrils[[i]][j+1] <- sim_results$myofibrils[[i]][j] - (used_protein * constants$phr)
          sim_results$mm[[i]][j + 1] <-
            sim_results$mitochondria[[i]][1] + (sim_results$myofibrils[[i]][j] - (used_protein *
                                                                                    constants$phr))
          sim_results$afm[[i]][j+1] <- sim_results$afm[[i]][j] - (EFromProtein / constants$ped)
          sim_results$bm[[i]][j + 1] <-
            sim_results$bm[[i]][j] - (used_fat - used_fat_equiv) - (used_protein *
                                                                      constants$phr) - (EFromProtein / constants$ped)
          sim_results$dist[[i]][j] <- sim_results$true_speed[[i]][j] * 360
          j <-  j + 1
        }
      }
    }
  }else {
    for (i in seq_len(nrow(data))) {
      #i = 1
      sim_results$bm[[i]][1] <- allMass[i]
      sim_results$fm[[i]][1] <- fatMass[i]
      sim_results$mm[[i]][1] <-  muscleMass[i]
      current_fm <- sim_results$fm[[i]][1]

      j <- 1
      while (sim_results$fm[[i]][j] > 0.000001) {
        # calculate speed
        sim_results$min_speed[[i]][j] <-
          .minpowspeed_cpp(
            bm = sim_results$bm[[i]][j],
            ws =wingSpan[i],
            ipf = constants$ipf,
            g = constants$g,
            airDensity = constants$airDensity,
            bdc = constants$bdf
          )
        # true speed not constant
        sim_results$true_speed[[i]][j] <- sim_results$min_speed[[i]][j] * constants$speedRatio

        # mechanical power from power curve holding true air-speed constant
        sim_results$mechPow[[i]][j] <-
          .pow.curve(bm = sim_results$bm[[i]][j],
                     ws =wingSpan[i],
                     wa = wingArea[i],
                     tas =  sim_results$true_speed[[i]][j], constants = constants)

        # wing frequency
        sim_results$wing_freq[[i]][j] <-
          .wingbeat.freq(bm = sim_results$bm[[i]][j],
                         ws =wingSpan[i],
                         wa = wingArea[i],
                         constants)

        # subdivide muscle mass into mitochondria and myofibrils
        sim_results$myofibrils[[i]][1] <-
          sim_results$mm[[i]][1] * (1 - constants$mipd * (sim_results$mechPow[[i]][1] /
                                                           sim_results$mm[[i]][1]) * constants$muscDensity)
        sim_results$mitochondria[[i]][j] <-
          sim_results$mm[[i]][1] - sim_results$myofibrils[[i]][1]

        # specific power at begining of flight (before fat has been consumed)
        specwork_start <-
          sim_results$mechPow[[i]][1] / (sim_results$myofibrils[[i]][1] * sim_results$wing_freq[[i]][1])

        # chemical power
        deltaE <- (sim_results$mechPow[[i]][j] / constants$mce) +
          .basal.met(
            constants = constants,
            mFrame = frame_mass[i],
            mMusc = sim_results$mm[[i]][j],
            ordo = taxon[i]
          )

        # increase deltaE by 10% respiratory
        sim_results$chemPow[[i]][j] <- deltaE + (deltaE * 0.1)

        # fat consumed in the interval?
        used_fat <- sim_results$chemPow[[i]][j] / constants$fed * 360

        #Reduce body composition by this consumed fat and determine what amount
        # of protein to consume to achieve specific power at start of flight
        dummy_bm <- sim_results$bm[[i]][j] - used_fat
        dummy_fm <- sim_results$fm[[i]][j] - used_fat

        # because of this reduction minimum speed, mechanical power, and
        # wing frequency reduce

        dummy_true_speed <-
          .minpowspeed_cpp(
            bm = dummy_bm,
            ws =wingSpan[i],
            ipf = constants$ipf,
            g = constants$g,
            airDensity = constants$airDensity,
            bdc = constants$bdf
          ) * constants$speedRatio

        dummy_mechPow <- .pow.curve(bm = dummy_bm, ws =wingSpan[i],
                                    wa = wingArea[i], tas = dummy_true_speed, constants = constants)

        # wing frequency
        dummy_wing_freq <-
          .wingbeat.freq(bm = dummy_bm, ws =wingSpan[i], wa = wingArea[i], constants)

        # specific work after reduction of fat
        dummy_specwork <- dummy_mechPow / (sim_results$myofibrils[[i]][j] * dummy_wing_freq)

        # amount of protein consumed that restores specific work to initial value
        used_protein <-
          -(dummy_mechPow / (dummy_wing_freq * specwork_start)) + sim_results$myofibrils[[i]][j]

        # checking if specific work has been restored
        sim_results$spec_work[[i]][j] <-
          dummy_mechPow / ((sim_results$myofibrils[[i]][j] - used_protein) * dummy_wing_freq)

        # amount of fuel energy released is found by multiplying the mass of dry protein
        # removed by the energy density of dry protein
        used_fat_equiv <- (used_protein * constants$ped) / constants$fed

        # adjust body components
        # new fat mass
        sim_results$fm[[i]][j + 1] <-  sim_results$fm[[i]][j] - (used_fat - used_fat_equiv)
        sim_results$myofibrils[[i]][j + 1] <- sim_results$myofibrils[[i]][j] - (used_protein * constants$phr)
        sim_results$mm[[i]][j + 1] <- sim_results$mitochondria[[i]][1] + (sim_results$myofibrils[[i]][j] - (used_protein * constants$phr))
        sim_results$bm[[i]][j + 1] <- sim_results$bm[[i]][j] - (used_fat - used_fat_equiv) - (used_protein * constants$phr)
        sim_results$dist[[i]][j] <- sim_results$true_speed[[i]][j] * 360
        j <-  j + 1

      }
    }
  }
  return(sim_results)
}
