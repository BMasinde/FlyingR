
####################################################################################
#' Metabolic power ratio function
#'
#' @author Brian Masinde
#' @param cons Constants defined
#' @param m body mass at start/end of flight
#' @param ws wing span
#' @param ord Order (passerine or non-passerine)
#' @return x2 (metabolic power ratio)

.met.pow.ratio <- function(cons, m, ws, ord) {
  # passerines index
  pind <- which(ord == 1)

  a <- rep(cons$alpha[[1]], length(ord))

  d <- rep(cons$delta[[1]], length(ord))

  # reassign non-passerines
  a[-pind] <- cons$alpha[[2]]

  d[-pind] <- cons$delta[[2]]

  num <-
    6.023 * a * cons$n * cons$airDensity ^ 0.5 * ws ^ 1.5 * m ^
    (d - (5 / 3))
  den <-  cons$k ^ (3 / 4) * cons$g ^ (5 / 3)

  x2 <- num / den

  return(x2)
}

######################################################################################
#' minimum power speed
#'
#' @author Brian Masinde
#' @name .min.pow.speed
#' @param m all bofy mass
#' @param ws wing span
#' @param cons constants
#' @return Vmp (minimum power speed)

.min.pow.speed <- function(m, ws, cons) {
  Vmp <- ((0.807 * cons$k ^ 0.25 * m ^ 0.5 * cons$g ^ 0.5) /
            (cons$airDensity ^ 0.5 * ws ^ 0.5 * .body.front.area(m) ^ 0.25 * cons$bdc ^
               0.25)
  )

  Vmp <- Vmp - (Vmp * 0.1)

  return(Vmp)
}

######################################################################################
#' body frontal area
#' @author Brian Masinde
#' @param  m body mass
#' @return body frontal area
#'
.body.front.area <- function(m) {
  Sb <- 0.00813 * m ^ 0.666

  return(Sb)
}

######################################################################################
#' disk.area
#' @author Brian Masinde
#' @param ws wing span
#'
.disk.area <- function(ws) {
  Sd <- (pi*(ws^2))/4
}

######################################################################################
#' maximum range speed
#'
#' @author Brian Masinde
#' @param m all up mass
#' @param ws wing span

.max.range.speed <- function(m, ws, cons) {
  num <- cons$k^0.25 * m^0.5 * cons$g^0.5

  den <- cons$airDensity^0.5 * (cons$bdc * .body.front.area(m))^0.25 * .disk.area(ws)^0.25

  Vmr <- num/den

  return(Vmr)
}

#########################################################################################
#' induced power in horizontal flight
#' @author Brian Masinde
#' @param m all up mass
#' @param ws wing span
#' @param Vt true airspeed
#' @param cons constants
#' @return induced power in horizontal flight


.induced.pow <- function(m, ws, Vt, cons) {
  sapply(Vt, function(x)
    if (x <= 0) {
      x  <-  0.1
      cat("minimum true airspeed zero for some obesrvations")
    })

  pind <- (2 * cons$k * (m * cons$g) ^ 2) / (Vt * pi * ws ^ 2 * cons$airDensity)

  return(pind)
}

######################################################################################
#' parasite power
#' @name parasite.pow
#' @author Brian Masinde
#' @param Vt true airspeed
#' @param m all up mass
#' @param cons constants
#' @return ppar

.parasite.pow <- function(m, Vt, cons) {
  ppar <- (cons$airDensity * Vt * .body.front.area(m) * cons$bdc) / 2

  return(ppar)
}

#########################################################################################
#' absolute minimum power
#'
#' @name abs.min.pow
#' @param m all up mass
#' @param ws wing span
#' @param cons constants
#' @return  pam (absolute minimum power for an ideal bird to fly at Vmp)


.abs.min.pow <- function(m, ws, cons) {
  pam <-
    (1.05 * cons$k ^ 0.75 * m ^ 1.5 * cons$g ^ 1.5 * .body.front.area(m) ^ 0.25 * cons$bdc) /
    (cons$airDensity ^ 0.5 * ws ^ 1.5)

  return(pam)
}

##############################################################################################
#' profile power ratio
#'
#' @name prof.pow.ratio
#' @author Brian Masinde
#' @param ws wing span
#' @param wa wing area
#' @return profile power ratio

.prof.pow.ratio <- function(ws, wa, cons) {
  # ws = wing span
  # wa = wing area
  X1 <- cons$ppcons / (ws ^ 2 / wa)

  return(X1)
}

##############################################################################################
#' profile power
#'
#' @author Brian Masinde
#' @name prof.pow
#' @param x1 profile power ratio
#' @param amp absolute minimum power
#' @return ppro
#'

.prof.pow <- function(x1, amp) {
  # x1 = profile power ratio
  # amp = results of absolute minimum power
  ppro <- x1 * amp

  return(ppro)
}

##############################################################################################
#' power curve loop procedure
#'
#' @inheritParams induced.pow
#' @inheritParams parasite.pow
#' @inheritParams prof.pow
#'


.pc.proc <- function(m, ws, wa, Vmp, cons) {

  # induced power at starting speed
  pind <- .induced.pow(m, ws, Vt = Vmp, cons)

  # parasite power
  ppar <- .parasite.pow(m, Vt = Vmp, cons)

  # profile power
  x1 <- .prof.pow.ratio(ws, wa, cons)
  amp <- .abs.min.pow(m, ws, cons)
  ppr <- .prof.pow(x1, amp)

  # total power
  tpow <- pind + ppar + ppr

  return(tpow)
}

##############################################################################################
#' power curve
#'
#' @author Brian Masinde
#' @name pow.curve
#' @inheritParams prof.pow
#' @inheritParams prof.pow.ratio
#' @inheritParams abs.min.pow
#' @inheritParams parasite.pow
#' @inheritParams .induced.pow
#' @inheritParams .body.front.area
#' @inheritParams .min.pow.speed

.pow.curve <- function(m, ws, wa, cons) {
  # minimum power speed for birds
  #m <- body_mass
  #ws <- wing_span
  #wa <- wing_area

  Vmp <- .min.pow.speed(m, ws, cons)

  for (i in 1:length(Vmp)) {
    init_pow <- ceiling(.pc.proc(m[i], ws[i], wa[i], Vmp[i], cons))

    nxt_pow <- ceiling(.pc.proc(m[i], ws[i], wa[i], Vmp[i] + 0.1, cons))

    j <- 2
    while (init_pow > nxt_pow) {
      init_pow <- nxt_pow
      nxt_pow <-
        ceiling(.pc.proc(m[i], ws[i], wa[i], Vmp[i] + (0.1 * j), cons))
      j <- j + 1
    }

    # lowest rate of muscular exertion required to fly
    Vmp[i] <- Vmp[i] + (0.1 * j)

  }

  # maximum range speed
  Vmr <- .max.range.speed(m, ws, cons)

  # # mechanical power btwn Vmp and Vmr
  #
  # mech_power <- list()
  # chem_power <- list()
  # for (i in length(Vmp)) {
  #   while (Vmp[i] < Vmr) {
  #     # mech power
  #    mech_power[[i]] <-  pc.proc(m[i], ws[i], wa[i], Vmp[i])
  #    chem_power[[i]] <- mech_power[[i]]/cons$n
  #    Vmp[i] <- Vmp[i] + 0.1
  #   }
  # }

  # return list of Vmp and Vmr
  return(list(Vmp, Vmr))
}

###############################################################################################







