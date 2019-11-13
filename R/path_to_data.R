#' path to data
#' @author Brian Masinde
#' @name .pathToData
#' @description Handles arguments for path to data.
#' @param fileArgs
#' @return data processed data from path
#' @importFrom  utils read.csv

.pathToData <- function(fileArgs = list(), ...) {
  # throw error wrong argument in fileArgs
  if(length(fileArgs) > 7){
    stop("Wrong argument in fileArgs", call. = FALSE)
  }

  defaults <- list(
    file = fileArgs$path,
    sep = ",",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    comment.char = ""
  )

  args <- c("sep", "quote", "dec", "fill", "comment.char")

  given <- which(args %in% names(fileArgs) == TRUE)

  argsGiven <- args[given]
  for (i in 1:length(argsGiven)) {
    defaults[argsGiven[i]] <- fileArgs[argsGiven[i]]
  }

  data <- read.csv(file = fileArgs$path, header = TRUE, sep = defaults$sep,
                   quote = defaults$quote, dec = defaults$dec, fill = defaults$fill,
                   comment.char = defaults$comment.char, ...)


  # preprocess data
  data <- .colnames.match(data)

  return(data)
}
