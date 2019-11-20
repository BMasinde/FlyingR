#' Constant muscle mass time marching computation
#' @author Brain Masinde.
#' @name constant_muscle_mass
#' @param data
#' @param constant
#' @param speed_control


.constant_muscle_mass <- function(data, constant, speed_control) {
  if (missing(data) == TRUE) {
    stop("Missing data argument")
  }

  if(missing(constant) == TRUE){
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
      while (currentFM > 0.00001) {
        # find speed and power
        simResults$minSpeed[[i]][j] <-
          .min.pow.speed(simResults$bm[[i]][j], wingSpan[i], cons)

        simResults$trueSpeed[[i]][j] <- simResults$minSpeed[[i]][j] * 1.2

        simResults$rvvmp[[i]][j] <-
          simResults$trueSpeed[[i]][j] / simResults$minSpeed[[i]][j]

        # mechanical power
        power <-
          .total.mech.power(simResults$bm[[i]][j],
                            wingSpan[i],
                            wingArea[i],
                            simResults$trueSpeed[[i]][j],
                            cons)

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
    print("No other method")
  }

  return(simResults)
}
