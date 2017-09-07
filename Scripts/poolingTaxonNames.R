

cover <- read.csv("CSVs/CoastalSitesCorrected_9042017.csv", stringsAsFactors = F)
head(cover)

library(dplyr)
library(tidyr)

taxons <- cover %>% group_by(site_code, Family, Taxon) %>%
  summarise(live = unique(live), 
            local_provenance = (unique(local_provenance)),
            local_lifeform = (unique(local_lifeform)),
            functional_group = (unique(functional_group)))

for(taxon.name in taxons$Taxon){
  tempdata <- taxons[taxons$Taxon == taxon.name,]
  if(length(unique(tempdata$live)) > 1){
    cat(print("Error in live status", taxon.name)
    )
  }
  if(length(unique(tempdata$local_provenance)) > 1){
    print("Fix local provenance:")
    print(tempdata)
    tempdata$local_provenance = as.character(readline())
  }
  if(length(unique(tempdata$local_lifeform))> 1){
    print("Fix local lifeform")
    print(tempdata)
    tempdata$local_lifeform = as.character(readline())
  }
  if(length(unique(tempdata$functional_group))> 1){
    print("Fix functional group")
    print(tempdata)
    tempdata$functional_group = as.character(readline())
  }
  
  taxons[taxons$Taxon == taxon.name,] <- tempdata
}

taxons <- taxons %>% group_by(Family, Taxon) %>%
  summarise(live = unique(live), 
            local_provenance = (unique(local_provenance)),
            local_lifeform = (unique(local_lifeform)),
            functional_group = (unique(functional_group)))

taxons$Taxon <- gsub(" ", "\\.", taxons$Taxon)

#write.csv(taxonomy, "CoastalSitesTaxons.csv")
