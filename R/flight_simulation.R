
#' Implementation of the methods 1 and 2 base on Pennycuick. Both use Breguet's equations for lift drag ratio calculation.
#' @author Brian Masinde
#'
#' @name flysim
#'
#' @param data A data frame or a list (for a single bird observation). See example
#' @param method Two methods are currently available: "breguet" and "breguet_adj"
#' @param consumption Applies for method "breguet_adj" only. Percentage of fuel to be consumed by
#' end of flight. Between 0-1.
#' @param ctrl
#'
#' @include misc_functions.R method_1.R method_2.R
#' @return S3 class object with range estimates based on methods defined and constants
#'
#' @export
#'
#' @examples
#' flysim(data = birds, method = "breguet", ctrl = list(air_dens = 1.11))
#' flysim(data = birds, method = "breguet_adj", consumption = 0.87)


flysim <- function(data, method, ctrl, ...) {
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

  # check order column
  # if (is.data.frame(data) == TRUE && is.factor(data[, 5]) == FALSE) {
  #   stop("Order column should be a factor with levels 1 or 2")
  # }

  if (is.data.frame(data) == TRUE && levels(data[, 5]) != c("1", "2")) {
    stop("Order column should be a factor with levels 1 or 2")
  }

  # missing method
  if (missing(method) == TRUE) {
    cat("breguet as default method \n")
    method <- "breguet"
  }


  # data extract structure ----------------------------------------
  if (is.data.frame(data)  == TRUE) {
    name <- as.vector(data[ ,1])
    body_mass <- data[, 2]
    wing_span <- data[, 3]
    fat_mass <- data[, 4]
    Order <- data[, 5]
    wing_area <- data[, 6]
  } else {
    name <- data[[1]]
    body_mass <- data[[2]]
    wing_span <- data[[3]]
    fat_mass <- data[[4]]
    Order <- data[[5]]
    wing_area <- data[[6]]
  }

  # Breguet method 1
  if (method == "breguet" &&
      missing(ctrl) == TRUE) {
    estimates <-
      breguet(body_mass, wing_span, fat_mass, Order, wing_area)
  } else if (method == "breguet" && missing(ctrl) == FALSE) {
    estimates <-
      breguet(body_mass, wing_span, fat_mass, Order, wing_area, ctrl)
  } else if (method == "breguet_adj" && missing(ctrl) == TRUE) {
    estimates <-
      breguet_adj(body_mass, wing_span, fat_mass, Order, wing_area, consumption)
  } else if (method == "breguet_adj" && missing(ctrl) == FALSE) {
    estimates <-
      breguet_adj(body_mass, wing_span, fat_mass, Order, wing_area, consumption)
  }

  # class -----------------------------------------------------------
  estimates$Range <- as.data.frame(cbind("name" = name, "Range" = round(estimates$Range, 3)))
  estimates$data <- data

  class(estimates) <- append(class(estimates), "flysim")

  # return class object
  return(estimates)
}



