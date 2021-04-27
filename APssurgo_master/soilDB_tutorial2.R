# Soil Data Access (SDA) Tutorial

setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

setwd('./Lopt_UR/Data/APssurgo-master')

library(soilDB)
library(sp)
library(rgdal)
library(plyr)
library(raster)
library(rgeos)

SiteName = 'lott_ols_2018'

app_map <- st_read('./boundaries/lott_ols_2018_appmap30and90.shp') %>%
  st_transform(crs = 4326)


