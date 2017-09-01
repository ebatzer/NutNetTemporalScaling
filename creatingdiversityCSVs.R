
source("genspecmat.R")
library(testthat)
library(dplyr)

plotdat <- read.csv("comb-by-plot-28-July-2017.csv", stringsAsFactors = F)
comdat <- read.csv("full-cover-28-July-2017.csv", stringsAsFactors = F)
biomass <- read.csv("full-biomass-28-July-2017.csv", stringsAsFactors = F)
envtl <- read.csv("comb-by-plot-clim-soil-diversity-28-Jul-2017.csv", stringsAsFactors = F)

site_subset <- c("mcla.us", "hopl.us", "sier.us")

plotdat <- plotdat[plotdat$site_code %in% site_subset,]

comdat <- comdat[comdat$site_code %in% site_subset,]

biomass <- biomass[biomass$site_code %in% site_subset,]

envtl <- envtl[envtl$site_code%in% site_subset,]

com.ids <- genSpecMat(comdat, idcols = c(1:8))[[1]]
com.mat <- genSpecMat(comdat, idcols = c(1:8))[[2]]

com.ids = addplotdata(com.ids = com.ids,
            plotdat = plotdat,
            grouping.vars = c("site_code", "plot"),
            merge.vars = c("site_code", "plot", "year", "block"))

com.ids = addplotdata(com.ids = com.ids,
            plotdat = select(biomass, -matches("live")) %>% spread(category, mass),            
            grouping.vars = c("site_code", "plot"),
            merge.vars = c("site_code", "plot", "year", "block"))

com.ids = addplotdata(com.ids = com.ids,
                      plotdat = envtl,            
                      grouping.vars = c("site_code", "plot"),
                      merge.vars = c("site_code", "plot", "year", "block"))

write.csv(x = com.mat, "NutNetCommunityMatrix.csv")
write.csv(x = com.ids, "NutNetCommunityIdentification.csv")
