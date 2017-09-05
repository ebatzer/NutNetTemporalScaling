# Computing hill diversity numbers for groups

compute_hilldiv <- function(com.matrix, # Community matrix
                            com.id, # Community matrix ID dataframe
                            divval = 1, # Diversity value, defaults to 1
                            groups,
                            idcols # Grouping variables 
                            ){
  
  # Required libraries
  require(dplyr);require(tidyr);require(vegetarian);require(testthat)
  
  # Making sure that row numbers match
  expect_true(nrow(com.matrix) == nrow(com.id), 
              "Error: Community matrix and ID dataframe are not the same number of rows!")

  # Merge datasets to make subsetting easier
  com.merge <- cbind(com.id[,idcols], com.matrix)
  
  # Create empty list for storage
  summarydata <- list()
  
  # Computes alpha individually for all rows
  compute_alpha_sites <- function(df, subset){
    hvec <- c()
    for(rows in 1:nrow(df)){
      hvec[rows] = H(df[rows,-subset], lev = "alpha")
    }
    return(hvec)
  }
  
  # Computes mean alpha, beta, and gamma for pooled sets of values
  compute_alpha <- function(df, subset){
    return(H(df[,-subset], lev = "alpha"))
  }
  compute_beta <- function(df, subset){
    return(H(df[,-subset], lev = "beta"))
  }
  compute_gamma <- function(df, subset){
    return(H(df[,-subset], lev = "gamma"))
  }
  
  # For each site
  for(site_no in 1:length(unique(com.id$site_code))){
    
    # Append a new blank value to the storage list
    summarydata <- c(summarydata, NA)
    
    # Subset community data based on plot
    com.subset <- com.merge[com.merge$site_code == unique(com.id$site_code)[site_no],]
    
    # Compute mean alpha, beta, and gamma diversity
    divstats = com.subset %>% 
      group_by_at(vars(one_of(groups)))%>% 
      do(data.frame(mean.alpha = compute_alpha(., subset = idcols),
                    beta = compute_beta(., subset = idcols),
                    gamma = compute_gamma(., subset = idcols)))
    
    # Compute general summary stats, append diversity values, and fill empty list cell
    summarydata[[site_no]][1] = list(data.frame(bind_cols(com.subset %>% 
      group_by_at(vars(one_of(groups)))%>% 
      summarise(year_start = min(year),
                trt = unique(trt),
                type = unique(na.omit(experiment_type)),
                obs = n()),
      divstats[,(ncol(divstats)-2):ncol(divstats)]
      )))
    

    # Compute individual alpha diversity stats and append to the list
    summarydata[[site_no]][2] = list(data.frame(site = com.subset$site_code,
                                           plot = com.subset$plot,
                                           year = com.subset$year,
                                           trt = com.subset$trt,
                                           alpha = compute_alpha_sites(com.subset, subset = idcols)))
    
  }
  
  # Return storage list
  return(summarydata)
  
}

mat <- read.csv("CSVs/NutNetCommunityMatrix.csv")[,-1]
ids <- read.csv("CSVs/NutNetCommunityIdentification.csv")[,-1]

compute_hilldiv(com.matrix = mat,
                com.id = ids,
                divval = 1,
                groups = c("site_code", "plot"),
                idcols = c(1:20))


