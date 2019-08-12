#' Metabolic power ratio function
#'
#' @author Brian Masinde
#' @param cons Constants defined
#' @param m body mass at start/end of flight
#' @param ws wing span
#' @return x2 (metabolic power ratio)

met_pow_ratio <- function(cons, m, ws) {
  # passerines index
  pind <- which(Order == 1)

  a <- rep(cons$alpha[[1]], length(Order))

  d <- rep(cons$delta[[1]], length(Order))

  # reassign non-passerines
  a[-pind] <- cons$alpha[[2]]

  d[-pind] <- cons$delta[[2]]

  num <-
    6.023 * a * cons$n * cons$air_dens ^ 0.5 * ws ^ 1.5 * m ^
    (d - (5 / 3))
  den <-  cons$k ^ (3 / 4) * cons$g ^ (5 / 3)

  x2 <- num / den

  return(x2)
}

#' minimum power speed
#'
#' @author Brian Masinde
#' @name min_pow_speed
#' @param m all bofy mass
#' @param ws wing span
#' @return Vmp (minimum power speed)

min_pow_speed <- function(m, ws, cons) {
  Vmp <- ((0.807 * cons$k ^ 0.25 * m ^ 0.5 * cons$g ^ 0.5) /
            (cons$air_dens ^ 0.5 * ws ^ 0.5 * body_front_area(m) ^ 0.25 * cons$bdc ^
               0.25)
  )

  Vmp <- Vmp - (Vmp * 0.1)

  return(Vmp)
}

#' body frontal area
#'
#' @param  m body mass
#' @return body frontal area
#'
body_front_area <- function(m) {
  Sb <- 0.00813 * m ^ 0.666

  return(Sb)
}

#' disk_area
#'
#' @param ws wing span
#'
disk_area <- function(ws) {
  Sd <- (pi*(ws^2))/4
}

#' maximum range speed
#'
#' @author Brian Masinde
#' @param m all up mass
#' @param ws wing span

max_range_speed <- function(m, ws, cons) {
  num <- cons$k^0.25 * m^0.5 * cons$g^0.5

  den <- cons$air_dens^0.5 * (cons$bdc * body_front_area(m))^0.25 * disk_area(ws)^0.25

  Vmr <- num/den

  return(Vmr)
}

#' induced power in horizontal flight
#'
#' @author Brian Masinde
#'
#' @param m all up mass
#'
#' @param ws wing span
#'
#' @param Vt true airspeed
#'
#' @return induced power in horizontal flight
#'


induced_pow <- function(m, ws, Vt) {
  sapply(Vt, function(x)
    if (x <= 0) {
      x  <-  0.1
      cat("minimum true airspeed zero for some obesrvations")
    })

  pind <-
    (2 * cons$k * (m * cons$g) ^ 2) / (Vt * pi * ws ^ 2 * cons$air_dens)

  return(pind)
}


#' parasite power
#'
#' @name parasite_pow
#' @author Brian Masinde
#' @param Vt true airspeed
#' @param m all up mass
#' @return ppar

parasite_pow <- function(m, Vt) {
  ppar <- (cons$air_dens * Vt * body_front_area(m) * cons$bdc) / 2

  return(ppar)
}


#' absolute minimum power
#'
#' @name abs_min_pow
#' @param m all up mass
#' @param ws wing span
#' @return  pam (absolute minimum power for an ideal bird to fly at Vmp)


abs_min_pow <- function(m, ws) {
  pam <-
    (1.05 * cons$k ^ 0.75 * m ^ 1.5 * cons$g ^ 1.5 * body_front_area(m) ^ 0.25 * cons$bdc) /
    (cons$air_dens ^ 0.5 * ws ^ 1.5)

  return(pam)
}


#' profile power ratio
#'
#' @name prof_pow_ratio
#' @author Brian Masinde
#' @param ws wing span
#' @param wa wing area
#' @return profile power ratio

prof_pow_ratio <- function(ws, wa, cons) {
  # ws = wing span
  # wa = wing area
  X1 <- cons$ppcons / (ws ^ 2 / wa)

  return(X1)
}


#' profile power
#'
#' @author Brian Masinde
#' @name prof_pow
#' @param x1 profile power ratio
#' @param amp absolute minimum power
#' @return ppro
#'

prof_pow <- function(x1, amp) {
  # x1 = profile power ratio
  # amp = results of absolute minimum power
  ppro <- x1 * amp

  return(ppro)
}

#' power curve loop procedure
#'
#' @inheritParams induced_pow
#' @inheritParams parasite_pow
#' @inheritParams prof_pow
#'


pc_proc <- function(m, ws, wa, Vmp) {

  # induced power at starting speed
  pind <- induced_pow(m, ws, Vt = Vmp)

  # parasite power
  ppar <- parasite_pow(m, Vt = Vmp)

  # profile power
  x1 <- prof_pow_ratio(ws, wa, cons)
  amp <- abs_min_pow(m, ws)
  ppr <- prof_pow(x1, amp)

  # total power
  tpow <- pind + ppar + ppr

  return(tpow)
}


#' power curve
#'
#' @author Brian Masinde
#' @name pow_curve
#' @inheritParams prof_pow
#' @inheritParams prof_pow_ratio
#' @inheritParams abs_min_pow
#' @inheritParams parasite_pow
#' @inheritParams induced_pow
#' @inheritParams body_front_area
#' @inheritParams min_pow_speed

pow_curve <- function(m, ws, wa) {
  # minimum power speed for birds
  m <- body_mass
  ws <- wing_span
  wa <- wing_area

  Vmp <- min_pow_speed(m, ws, cons)

  for (i in 1:length(Vmp)) {
    init_pow <- ceiling(pc_proc(m[i], ws[i], wa[i], Vmp[i]))

    nxt_pow <- ceiling(pc_proc(m[i], ws[i], wa[i], Vmp[i] + 0.1))

    j <- 2
    while (init_pow > nxt_pow) {
      init_pow <- nxt_pow
      nxt_pow <-
        ceiling(pc_proc(m[i], ws[i], wa[i], Vmp[i] + (0.1 * j)))
      j <- j + 1
    }

    # lowest rate of muscular exertion required to fly
    Vmp[i] <- Vmp[i] + (0.1 * j)
  }

  # maximum range speed
  Vmr <- max_range_speed(m, ws, cons)

  # # mechanical power btwn Vmp and Vmr
  #
  # mech_power <- list()
  # chem_power <- list()
  # for (i in length(Vmp)) {
  #   while (Vmp[i] < Vmr) {
  #     # mech power
  #    mech_power[[i]] <-  pc_proc(m[i], ws[i], wa[i], Vmp[i])
  #    chem_power[[i]] <- mech_power[[i]]/cons$n
  #    Vmp[i] <- Vmp[i] + 0.1
  #   }
  # }


}









