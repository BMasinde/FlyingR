#' Renge estimation by constant specific work hypothesis


csw <- function(data, control = list()) {

  # objects
  results <- list("data" = data,
                  "range" = vector(),
                  "fuel" = vector(),
                  #"Vmp" = vector(),
                  #"Vmr" = vector(),
                  "constants" = list()
  )

  # column match
  cols <- .colnames.match(names(data))

  # return object of class flysim
  class(results) <- append(class(results), "csr")
  # return class object
  return(results)
}
