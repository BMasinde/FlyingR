#' @title Range Estimation
#' @description  Practical range estimation of birds using methods in Pennycuik (1975)
#' Mechanics of Flight. These methods are based on Breguet equations.
#'
#' @author Brian Masinde
#'
#' @name flysim
#'
#' @param data A data frame.
#' @param ctrl A list for re-defining constants. See details.
#'
#' @details The option *ctrl takes the folowing arguments
#' \itemize{
#'    \item ppcons: Profile power constant
#'    \item energy: Energy content of fuel from fat
#'    \item g: Accelaration due to gravity
#'    \item n: Mechanical conversion efficiency [0,1]
#'    \item k: Induced power factor
#'    \item R: Ventilation and circulation power
#'    \item airDensity: Air density at cruising altitude
#'    \item bdc: Body drag coefficient
#'    \item alpha: Basal metabolism factors in passerines and non passerines
#'    \item delta: Basal metabolism factors in passerines and non passerines
#'    alpha*bodyMass^delta
#'    \item consume: Percentage of fuel to be used from fat mass
#'}
#' @include misc_functions.R lookup_table2.R input_match.R method_1.R method_2.R
#' @return S3 class object with range estimates based on methods defined and
#'        constants
#' \itemize{
#'    \item data as a data frame
#'    \item range estimates
#'    \item fuel
#'    \item constants (list)
#' }
#'
#' @importFrom dplyr filter
#'
#' @export flysim
#'
#' @examples
#' flysim(data = birds, ctrl = list(energy = 3.89*10^7))
#' flysim(data = birds,  ctrl = list(airDensity = 0.905))
#'
#' @usage flysim(data, ctrl = list())


flysim <- function(data, ctrl = list()) {
  # ... extra arguments to be passed to methods

  ##  Error check data #########################################################
  if (is.data.frame(data) == FALSE & is.list(data) == FALSE) {
    stop(">> data as list or data frame <<")
  }

  # check number of columns
  if (is.data.frame(data) == TRUE && ncol(data) < 6) {
    stop(">> at least 5 columns for data <<")
  }
  # check number of fields
  if (is.list(data) == TRUE && length(data) < 6) {
    stop("data list should have at least 4 fields")
  }

  # column match
  cols <- .colnames.match(names(data))

  results <- list("data" = data,
                  "range" = vector(),
                  "fuel" = vector(),
                  #"Vmp" = vector(),
                  #"Vmr" = vector(),
                  "constants" = list()
                  )

  ## compute data/list p/np ####################################################

  if (is.data.frame(data)  == TRUE) {

    if (sum(levels(data[, cols$order]) == levels(factor(c(1, 2)))) != 2) {
      stop("Order column should be a factor with levels 1 or 2", call. = FALSE)
    }

    smallPasserines <-
      dplyr::filter(data, cols$order == 1 & cols$bodyMass <= 0.05)

    id_sp <- which(cols$order == 1 & cols$bodyMass <= 0.05)

    nonSmallPasserines <-
      dplyr::filter(data, cols$order == 2 | cols$bodyMass >= 0.05)

    id_np <- which(cols$order == 2 | cols$bodyMass >= 0.05)
    # args small passerines
    argsSmallBird <- list("bodyMass" = smallPasserines[, cols$bodyMass],
                   "wingSpan" = smallPasserines[, cols$wingSpan],
                   "fatMass" = smallPasserines[, cols$fatMass],
                   "ordo" = smallPasserines[, cols$order],
                   "wingArea" = smallPasserines[, cols$wingArea]
                   #"name" = as.vector(smallPasserines[ ,1]
                   )

    # args big birds
    argsBigBird <- list("bodyMass" = nonSmallPasserines[, cols$bodyMass],
                   "wingSpan" = nonSmallPasserines[, cols$wingSpan],
                   "fatMass" = nonSmallPasserines[, cols$fatMass],
                   "ordo" = nonSmallPasserines[, cols$order],
                   "wingArea" = nonSmallPasserines[, cols$wingArea]
                   #"name" = as.vector(nonSmallPasserines[ ,1])
                   )

    if (nrow(smallPasserines) > 0 & nrow(nonSmallPasserines) > 0) {

      if (missing(ctrl) == TRUE) {

        smallBirds <- do.call(.breguet, args = argsSmallBird)
        bigBirds <- do.call(.breguet_adj, args = argsBigBird)
        results$constants <- bigBirds[[3]]

      }else {
        argsSmallBird$ctrl <- ctrl
        argsBigBird$ctrl <- ctrl
        smallBirds <- do.call(.breguet, args = argsSmallBird)
        #constants <- smallBirds[[3]]
        bigBirds <- do.call(.breguet_adj, args = argsBigBird)
        results$constants <- bigBirds[[3]]
      }
      # consitent order of results and input data
      names(smallBirds[[1]]) <- id_sp
      results$fuel[id_sp] <- smallBirds[[2]]
      names(bigBirds[[1]]) <- id_np
      results$fuel[id_np] <- bigBirds[[2]]
      range <- c(smallBirds[[1]],bigBirds[[1]])
      results$range <- range[order(as.numeric(names(range)))]
    }else if(nrow(smallPasserines) == 0) {

      if(missing(ctrl) == TRUE){
        bigBirds <- do.call(.breguet_adj, args = argsBigBird)
        results$constants <- bigBirds[[3]]
        results$range <- bigBirds[[1]]
        results$fuel <- bigBirds[[2]]
      }else {
        argsBigBird$ctrl <- ctrl
        bigBirds <- do.call(.breguet_adj, args = argsBigBird)
        results$constants <- bigBirds[[3]]
        results$range <- bigBirds[[1]]
        results$fuel <- bigBirds[[2]]
      }
    } else if(nrow(nonSmallPasserines) == 0) {
      if(missing(ctrl) == TRUE){
        smallBirds <- do.call(.breguet, args = argsSmallBird)
        results$constants <- smallBirds[[3]]
        results$range <- smallBirds[[1]]
        results$fuel <- smallBirds[[2]]
      }else {
        argsSmallBird$ctrl <- ctrl
        smallBirds <- do.call(.breguet, args = argsSmallBird)
        results$constants <- smallBirds[[3]]
        results$range <- smallBirds[[1]]
        results$fuel <- smallBirds[[2]]
      }
    }

  } else {
    args <- list("bodyMass" = data[[cols$bodyMass]],
                 "wingSpan" = data[[cols$wingSpan]],
                 "fatMass" = data[[cols$fatMass]],
                 "ordo" = data[[cols$order]],
                 "wingArea" = data[[cols$wingArea]]
                 )

    if (args$ordo == 1) {
      if (missing(ctrl) == TRUE) {
        smallBirds <- do.call(.breguet, args = args)
      }else {
        args$ctrl <- ctrl
        smallBirds <- do.call(.breguet, args = args)
      }
      results$range <- smallBirds[[1]]
      results$fuel <- smallBirds[[2]]
      results$constants <- smallBirds[[3]]
    }

    if (args$ordo == 2) {
      if (missing(ctrl) == TRUE) {
        bigBirds <- do.call(.breguet_adj, args = args)
      }else {
        args$ctrl <- ctrl
        bigBirds <- do.call(.breguet_adj, args = args)
      }
      results$range <- bigBirds[[1]]
      results$fuel <- bigBirds[[2]]
      results$constants <- bigBirds[[3]]
    }

  }

  # return object of class flysim
  class(results) <- append(class(results), "flysim")
  # return class object
  return(results)
}



