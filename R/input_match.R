
# identify columns from data
.colnames.match <- function(names) {
  # name or id column
  name <- grep("id|species.name|name|species name|species_name", names)
  wingSpan <- grep("ws|wing.span|wing_span|wing span|wingspan",
                   ignore.case = TRUE, names)
  wingArea <- grep("wa|wing.area|wing area|wingarea|wing_area",
                   ignore.case = TRUE,names)
  ordo <- grep("order|ordo|ord", ignore.case = TRUE, names)
  bmass <- grep("body.mass|empty.mass|all-up_mass|allupmass|body_mass|bodymass",
                ignore.case = TRUE, names)
  fmass <- grep("fat.mass|fatmass|fat_mass",
                ignore.case = TRUE,names)
  mmass <- grep("muscle.mass|musclemass|muscle_mass",
                ignore.case = TRUE,names)

  names <- list("name" = name,
                "bodyMass" = bmass,
                "wingSpan" = wingSpan,
                "fatMass" = fmass,
                "order" = ordo,
                "wingArea" =wingArea,
                "muslceMass"  = mmass)

  matches <- sapply(names, length)

  # throw error mutliple matches
  if(any(matches > 1) == TRUE) {
    stop("One column per variable")
  }

  if(any(matches == 0) == TRUE) {
    mismatch <- which(matches == 0)
    stop(paste(length(mismatch), "column(s) missing", sep = " "))
  }

  return(names)
}
