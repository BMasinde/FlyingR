#' @title Stopover mass calculator
#' @description During stop-overs birds replenish fat mass. Using simplifications
#' from Lindstöm 1991.
#' @name stopover.mass.calculator
#' @param file The name of the file which the data are to read from
#' @param header Logical. If TRUE use first row as column headers
#' @param sep separator
#' @param quote The set of quoting characters. see read.csv
#' @param dec The character used in the file for decimal points
#' @param fill See read.csv
#' @param comment.char For more details see read.csv
#' @param ... further arguments see read.csv
#' @param data A data frame
#' @param duration Days spent at stopover
#' @return body mass, fat mass, fat fraction,

# SOURCE #######################################################################
# Lindström, Å. (1991). Maximum Fat Deposition Rates in Migrating Birds.
# Ornis Scandinavica (Scandinavian Journal of Ornithology), 22(1),
# 12-19. doi:10.2307/3676616
################################################################################

stopover.mass.calculator <-
  function(file,
           header = TRUE,
           sep = ",",
           quote = "\"",
           dec = ".",
           fill = TRUE,
           comment.char = "",
           ...,
           data = NULL,
           duration = 1L) {
    # check file and data are not given together
    if (missing(file) == TRUE & is.null(data) == TRUE) {
      stop("Data not found \n", call. = TRUE)
    }

    # check duration is an non-zero integer
    if (is.integer(duration) == FALSE | duration == 0) {
      stop("duration is a non-integer, see doc \n")
    }

    # if file is given read.csv
    if (missing(file) == FALSE) {
      dataRead <- read.csv(file = file, header = header, sep = sep, quote = quote,
                           dec = dec, fill = fill, comment.char,
                           stringsAsFactors = FALSE, ...)
      data <- .colnames.match(dataRead)
    } else {
      data <- .colnames.match(data)
    }

    # Get lean mass
    leanMass <- data$allMass - data$fatMass

    # maximum fat deposition rate based on order
    maxFatDepositionRate <-
      ifelse(data$taxon == 1, 2.22 * leanMass ^ (-0.27), 2.80 * leanMass ^ (-0.27))

    # check maxFatDepositionRate is not less than zero and not greater than hundred
    if (any(maxFatDepositionRate < 0) | any(maxFatDepositionRate > 100)) {
      warning("Maximum fat deposition rate contains values out of bounds [0,1]")
    }

    fatMassGained <- leanMass * (maxFatDepositionRate/100) * duration

    data$allMass <- data$allMass + fatMassGained
    data$fatMass <- data$fatMass + fatMassGained

    return(data)
  }

# SAMPLE RESULTS FROM FLIGHT ###################################################
# garden warbler
# fatMass = 0.0066
# bodyMass = 0.022
# duration = 24hrs
# intakeRate = 4.5
# existenceRatio =  3
# Results 0.0233 body mass, 0.00757 fat mass
# DME = daily metabolised energy intake
# DEE = daily energy expenditure
################################################################################
