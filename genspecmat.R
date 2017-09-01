# Generating species matrix:
genSpecMat <- function(comdat_long,
                       idcols,
                       spec.colname = NULL,
                       cover.colname = NULL){
  
  require(testthat)
  require(tidyr)
  require(dplyr)

  # Reassigns species and cover names if not provided
  if(is.null(spec.colname)){
    spec.colname = "Taxon"
    print("Using spec.colname = 'Taxon', NutNet default")
  }
  if(is.null(cover.colname)){
    cover.colname = "max_cover"
    print("Using cover.colname = 'max_cover', NutNet default")
  } 
  
  # Converting character ID columns to numeric and making sure they match existing columns
  if(is.character(idcols)){
    idnum = grep(idcols, colnames(comdat_long))
    expect_true(length(idnum) == length(idcols), "Not all id column values match with column names")
  }else if(is.numeric(idcols)){
    idnum = idcols
  }else{
    stop("idcols are not specified as numeric or character string")
  }

  # Species ID and percent cover must correspond to a single column each
  expect_true(ncol(select(comdat_long, spec.colname)) == 1, 
              paste(spec.colname, "does not match a single column"))
  expect_true(ncol(select(comdat_long, cover.colname)) == 1, 
              paste(cover.colname, "does not match a single column"))
  
  # Species ID and percent cover must be a character and numeric, respectively
  expect_true(is.character(select(comdat_long, spec.colname)[,1]), 
              paste(spec.colname, "does not match a character column"))
  expect_true(is.numeric(select(comdat_long, cover.colname)[,1]), 
              paste(cover.colname, "does not match a numeric column"))
  
  # ID Columns specified must not overlap with species ID and percent cover cols
  expect_false(spec.colname %in% colnames(comdat_long[idnum]))
  expect_false(cover.colname %in% colnames(comdat_long[idnum]))
  
  # Removes columns that are not ID columns, species names, or cover values
  comdat_long <- comdat_long[c(idcols,
                               grep(spec.colname, colnames(comdat_long)),
                               grep(cover.colname, colnames(comdat_long)))]

  # Casting dataset from long to wide
  comdat_wide = spread(comdat_long, spec.colname, cover.colname)
  
  # Pulling out ID columns
  comdat_ids = comdat_wide[,idcols]

  # Pulling out species matrix, converts NAs to 0
  comdat_matrix = comdat_wide[,-idcols]
  comdat_matrix[is.na(comdat_matrix)] = 0 
  
  # Returns list of ID dataframe and community matrix
  return(list(sample_ids = comdat_ids,
              community_matrix = comdat_matrix))
}

