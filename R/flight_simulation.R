
#' Implementation of the methods 1 and 2 base on Pennycuick. Both use Breguet's
#' equations for lift drag ratio calculation.
#' @author Brian Masinde
#'
#' @name flysim
#'
#' @param data A data frame or a list (for a single bird observation). See example
#' @param method Two methods are currently available: "breguet" and "breguet_adj"
#' @param ctrl A list of re-definition of constants (*airDensity*,
#'             *consume*, *enegry e*, *mechanical efficiency n*).
#'
#' @include misc_functions.R lookup_table2.R method_1.R method_2.R
#' @return S3 class object with range estimates based on methods defined and
#'        constants
#'
#' @export flysim
#'
#' @examples
#' flysim(data = birds, ctrl = list(energy = 3.89*10^7))
#' flysim(data = birds, method = "breguet", ctrl = list(airDensity = 0.905))
#' flysim(data = birds, method = "breguet_adj", ctrl = list(airDensity = 0.905))


flysim <- function(data, method, ctrl) {
  # ... extra arguments to be passed to methods

  # Error check data--------------------------------------------
  if (is.data.frame(data) == FALSE & is.list(data) == FALSE) {
    stop("data input allows for dataframe or list")
  }

  # check number of columns
  if (is.data.frame(data) == TRUE && ncol(data) < 6) {
    stop("data should have at least 5 columns")
  }
  # check number of fields
  if (is.list(data) == TRUE && length(data) < 6) {
    stop("data list should have at least 4 fields")
  }

  if (is.data.frame(data) == TRUE && levels(data[, 5]) != c("1", "2")) {
    stop("Order column should be a factor with levels 1 or 2")
  }

  # missing method
  if (missing(method) == TRUE) {
    message("## Default method = 'breguet' \n \n")
    method <- "breguet"
  } else if (method != "breguet" || method != "breguet_adj") {
    stop("method = 'breguet' or 'breguet_adj' ")
  }


  # data extract structure ----------------------------------------
  if (is.data.frame(data)  == TRUE) {
    name <- as.vector(data[ ,1])
    bodyMass <- data[, 2]
    wingSpan <- data[, 3]
    fatMass <- data[, 4]
    ordo <- data[, 5]
    wingArea <- data[, 6]
  } else {
    name <- data[[1]]
    bodyMass <- data[[2]]
    wingSpan <- data[[3]]
    fatMass <- data[[4]]
    ordo <- data[[5]]
    wingArea <- data[[6]]
  }

  # Breguet method 1
  if (method == "breguet" &&
      missing(ctrl) == TRUE) {
    estimates <-
      .breguet(bodyMass, wingSpan, fatMass, ordo, wingArea)
  } else if (method == "breguet" && missing(ctrl) == FALSE) {
    estimates <-
      .breguet(bodyMass, wingSpan, fatMass, ordo, wingArea, ctrl)
  } else if (method == "breguet_adj" && missing(ctrl) == TRUE) {
    estimates <-
      .breguet_adj(bodyMass, wingSpan, fatMass, ordo, wingArea)
  } else if (method == "breguet_adj" && missing(ctrl) == FALSE) {
    estimates <-
      .breguet_adj(bodyMass, wingSpan, fatMass, ordo, wingArea, ctrl)
  }

  # class -----------------------------------------------------------
  estimates$Range <- as.data.frame(cbind("name" = name,
                                         "Range" = round(estimates$Range, 3)))
  estimates$data <- data

  class(estimates) <- append(class(estimates), "flysim")

  # return class object
  return(estimates)
}



