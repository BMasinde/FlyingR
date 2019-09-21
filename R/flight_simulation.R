
#' Implementation of the methods 1 and 2 base on Pennycuick. Both use Breguet's
#' equations for lift drag ratio calculation.
#' @author Brian Masinde
#'
#' @name flysim
#'
#' @param data A data frame or a list (for a single bird observation). See example
#' @param ctrl A list of re-definition of constants (*airDensity*,
#'             *consume*, *enegry e*, *mechanical efficiency n*).
#'
#' @include misc_functions.R lookup_table2.R method_1.R method_2.R
#' @return S3 class object with range estimates based on methods defined and
#'        constants
#' @importFrom dplyr filter
#'
#' @export flysim
#'
#' @examples
#' flysim(data = birds, ctrl = list(energy = 3.89*10^7))
#' flysim(data = birds,  ctrl = list(airDensity = 0.905))


flysim <- function(data, ctrl) {
  # ... extra arguments to be passed to methods

  ##  Error check data #########################################################
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

  if (is.data.frame(data) == TRUE &&
      sum(levels(data[, 5]) == levels(factor(c(1, 2)))) != 2) {
    stop("Order column should be a factor with levels 1 or 2")
  }

  # # check method
  # if (missing(method) == TRUE) {
  #   message("## Default method = 'breguet' \n \n")
  #   method <- "breguet"
  # } else if (method != "breguet" & method != "breguet_adj") {
  #   stop("method = 'breguet' or 'breguet_adj' ")
  # }

  results <- list("data" = data,
                  "range" = data.frame(),
                  "fuel" = vector(),
                  #"Vmp" = vector(),
                  #"Vmr" = vector(),
                  "constants" = list()
                  )

  ## compute data/list p/np ####################################################

  if (is.data.frame(data)  == TRUE) {
    smallPasserines <-
      dplyr::filter(data, data[, 5] == 1 & data[, 2] <= 0.05)

    id_sp <- which(data[, 5] == 1 & data[, 2] <= 0.05)

    nonSmallPasserines <-
      dplyr::filter(data, data[, 5] == 2 | data[, 2] >= 0.05)

    id_np <- which(data[, 5] == 2 | data[, 2] >= 0.05)
    # args small passerines
    argsSmallBird <- list("bodyMass" = smallPasserines[, 2],
                   "wingSpan" = smallPasserines[, 3],
                   "fatMass" = smallPasserines[, 4],
                   "ordo" = smallPasserines[, 5],
                   "wingArea" = smallPasserines[, 6],
                   "name" = as.vector(smallPasserines[ ,1])
    )

    # args big birds
    argsBigBird <- list("bodyMass" = nonSmallPasserines[, 2],
                   "wingSpan" = nonSmallPasserines[, 3],
                   "fatMass" = nonSmallPasserines[, 4],
                   "ordo" = nonSmallPasserines[, 5],
                   "wingArea" = nonSmallPasserines[, 6],
                   "name" = as.vector(nonSmallPasserines[ ,1]))

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
      rownames(smallBirds[[1]]) <- id_sp
      results$fuel[id_sp] <- smallBirds[[2]]
      rownames(bigBirds[[1]]) <- id_np
      results$fuel[id_np] <- bigBirds[[2]]
      range <- rbind(smallBirds[[1]],bigBirds[[1]])
      results$range <- range[order(as.numeric(row.names(range))), ]
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
    args <- list("bodyMass" = data[[2]],
                 "wingSpan" = data[[3]],
                 "fatMass" = data[[4]],
                 "ordo" = data[[5]],
                 "wingArea" = data[[6]],
                 "name" =  data[[1]])

    if (args$ordo == 1) {
      if (missing(ctrl) == TRUE) {
        smallBirds <- do.call(.breguet, args = args)
      }else {
        args$ctrl <- ctrl
        smallBirds <- do.call(.breguet, args = args)
      }
      results$Range <- smallBirds[[1]]
      results$fuel <- smallBirds[[2]]
      results$constant <- smallBirds[[3]]
    }

    if (args$ordo == 2) {
      if (missing(ctrl) == TRUE) {
        bigBirds <- do.call(.breguet_adj, args = args)
      }else {
        args$ctrl <- ctrl
        bigBirds <- do.call(.breguet_adj, args = args)
      }
    }
    results$Range <- bigBirds[[1]]
    results$fuel <- bigBirds[[2]]
    results$constant <- bigBirds[[3]]
  }


  #results$constants <- constants

  # return object of class flysim
  class(results) <- append(class(results), "flysim")

  # return class object
  return(results)
}



