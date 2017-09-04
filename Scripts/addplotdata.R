# Generating large ID dataframe, containing plot IDs, productivity, and weather data

addplotdata <- function(com.ids, # Community ID output from genspecmat function
                        plotdat, # Other data to be merged to community ID output
                        grouping.vars, # What variables are used to group datasets? Character string
                        merge.vars # What variables are used to merge datasets? Character string
                        ){
  
  # Number of observations per unique site-plot combination
  obs.plot <- plotdat %>%
    group_by_at(vars(one_of(grouping.vars))) %>% 
    summarize(obs = n())
  
  obs.com <- com.ids %>% 
    group_by(site_code, plot) %>% 
    summarize(obs = n()) 
  
  obs.com$diff = obs.com$obs - obs.plot$obs
  
  if(nrow(com.ids) != nrow(plotdat)){
    print(paste("The following site-plot combinations are mismatched in their number of observations:",
                     toString(unlist(t(obs.com %>% filter(diff != 0))))))
  }
  
  ids.merged <- merge(com.ids, plotdat, by = merge.vars)
  
  # Removes duplicated column names
  names(ids.merged) <- gsub("\\..", "", names(ids.merged))
  ids.merged <- ids.merged %>% subset(., select=which(!duplicated(names(.))))
  
  return(ids.merged)
}


