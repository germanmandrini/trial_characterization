# Load libraries and functions #################################################
#
# install.packages(c("shiny","leaflet","shinythemes","FedData","maps",
#                   "maptools","rgdal","raster","dplyr","ggplot2","XML",
#                   "Hmisc","lubridate"))
# install.packages('shiny')
# library(shiny)
# install.packages('leaflet')
# library(leaflet)
# install.packages('shinythemes')
# library(shinythemes)
# library(FedData)
library(maps)
library(maptools)
# library(rgdal)
# library(raster)
# library(dplyr)
# library(ggplot2)
# library(XML)
# library(Hmisc)
# library(lubridate)

setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

grid10_fields_sf <- readRDS('./vr_value/Data/Grid/grid10_fields_sf.rds')

# source("R/moldSSURGO_gm.R")

#---------------------------------------------------------------
# Step 1 Get the soils for each field

all_soils <- list()

grid10_soils_list <- list()

for(tile_n in unique(grid10_fields_sf$id_tile)){
  # tile_n = 15
  print(tile_n)
  
  one_tile_sf <- grid10_fields_sf[grid10_fields_sf$id_tile == tile_n,]
  nrow(one_tile_sf)
  # if(nrow(one_tile_sf) >10){one_tile_sf <- one_tile_sf[1:10,]}
  
  source('./vr_value/Data/APssurgo_master/R/get_soils_parallel.R')

  grid10_soils_list[[tile_n]] <- fields_sf
}

#---------------------------------------------------------------
# Merge and save the fields with their soils
grid10_soils_sf1 <- do.call(what = base::rbind, args = grid10_soils_list)
saveRDS(grid10_soils_sf1, "./vr_value/Data/Grid/grid10_soils_sf1.rds")
grid10_soils_sf1 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf1.rds")

# Clean the soils (replace small areas by biggest)
source('./vr_value/Codes/clean_fields_step1.R')
grid10_soils_sf3 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf3.rds")
length(unique(grid10_soils_sf3$mukey)) #total soils to download

#---------------------------------------------------------------  
# Step 2 get the horizons information
source('./vr_value/Data/APssurgo_master/R/get_horizons_parallel.R')
grid10_horizons_v1_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v1_dt.rds")

# Clean the soils not available in SSURGO
source('./vr_value/Codes/clean_fields_step2.R')
grid10_soils_sf4 <- readRDS("./vr_value/Data/Grid/grid10_soils_sf4.rds")  


#---------------------------------------------------------------
# Step 3 Add more information and save
info <- data.table(grid10_soils_sf4, st_coordinates(st_centroid(grid10_soils_sf4))) %>% .[, .(id_tile, mukey, X, Y)]
info <- info[,.(id_tile = min(id_tile), X = mean(X), Y = mean(Y)), by = .(mukey)] #remove repeated mukeys in same tile

# Add the id_tile to make folders based on it. If a mukey is in more than 1 tile, it will be located in the lower id_tile
grid10_horizons_v2_dt <- merge(info, grid10_horizons_v1_dt, by = 'mukey')
setcolorder(grid10_horizons_v2_dt, c('id_tile','mukey', 'X', 'Y'))
saveRDS(grid10_horizons_v2_dt, "./vr_value/Data/Grid/grid10_horizons_v2_dt.rds")
