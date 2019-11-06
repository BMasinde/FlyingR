
# identify columns from data
.colnames.match <- function(names) {
  id <- grepl("id", names)

  name <- grepl("species.name|name|species name|species_name|scientific.name", names)

  wingSpan <- grepl("ws|wing.span|wing_span|wing span|wingspan",
                   ignore.case = TRUE, names)

  wingArea <- grepl("wa|wing.area|wing area|wingarea|wing_area",
                   ignore.case = TRUE,names)

  taxon <- grepl("order|ordo|ord|taxon", ignore.case = TRUE, names)

  bMass <- grepl("body.mass|empty.mass|all-up_mass|allupmass|body_mass|bodymass",
                ignore.case = TRUE, names)

  fMass <- grepl("fat.mass|fatmass|fat_mass",
                ignore.case = TRUE,names)

  mMass <- grepl("muscle.mass|musclemass|muscle_mass",
                ignore.case = TRUE,names)

  # only one with bird names
  if (sum(name) > 2) {
    stop("multiple name columns found. Remove one", call. = FALSE)
  }

  # missing column with id and name, set to false to gen
  # if (sum(id) == 0 & sum(name) == 0) {
  #   id == FALSE
  # }

  if (sum(taxon) == 0) {
    stop("taxon column not found", call. = FALSE)
  }

  if (sum(bMass) == 0) {
    stop("body mass not found", call. = FALSE)
  }

  if (sum(fMass) == 0) {
    stop("fat mass not found", call. = FALSE)
  }

  if (sum(mMass) == 0) {
    warning("muscle mass not found", call. = FALSE)
  }




  colIndex <- list("name" = which(name == TRUE),
                "bodyMass" = which(bMass == TRUE),
                "wingSpan" = which(wingSpan == TRUE),
                "fatMass" = which(fMass == TRUE),
                "order" = which(taxon == TRUE),
                "wingArea" = which(wingArea == TRUE),
                "muscleMass"  = which(mMass == TRUE),
                "id" = which(id == TRUE)
                )

  return(colIndex)
}
