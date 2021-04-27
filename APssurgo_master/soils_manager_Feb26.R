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

for(tile_n in unique(grid10_fields_sf$id_tile)[1:3]){
  # tile_n = 15
  print(tile_n)
  
  one_tile_sf <- grid10_fields_sf[grid10_fields_sf$id_tile == tile_n,]
  nrow(one_tile_sf)
  if(nrow(one_tile_sf) >10){one_tile_sf <- one_tile_sf[1:10,]}
  
  source('./vr_value/Data/APssurgo_master/R/get_soils_parallel.R')

  grid10_soils_list[[tile_n]] <- fields_sf
}

#---------------------------------------------------------------
# Merge and save the fields with their soils
grid10_soils_sf <- do.call(what = base::rbind, args = grid10_soils_list)
saveRDS(grid10_soils_v1_sf, "./vr_value/Data/Grid/grid10_soils_sf.rds")
grid10_soils_v1_sf <- readRDS("./vr_value/Data/Grid/grid10_soils_sf.rds")

# Clean the soils (replace small areas by biggest)
source('./vr_value/Data/APssurgo_master/R/clean_fields_step1.R')
grid10_soils_v2_sf <- readRDS("./vr_value/Data/Grid/grid10_soils_v2_sf")

#---------------------------------------------------------------  
# Step 2 get the horizons information
source('./vr_value/Data/APssurgo_master/R/get_horizons_parallel.R')
grid10_horizons_v1_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v1_dt.rds")

# Clean the soils not available in SSURGO
source('./vr_value/Data/APssurgo_master/R/clean_fields_step2.R')
grid10_soils_v3_sf <- readRDS("./vr_value/Data/Grid/grid10_soils_v3_sf")  

#---------------------------------------------------------------  
# Step 3 Calculate apsim variables
source('./vr_value/Data/APssurgo_master/R/calc_apsim_variables_parallel.R')
grid10_horizons_v2_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v2_dt.rds")


#---------------------------------------------------------------
# Step 3 Add more information and save
info <- data.table(grid10_soils_v3_sf, st_coordinates(st_centroid(grid10_soils_v3_sf))) %>% .[, .(id_tile, mukey, X, Y)]
info <- info[,.(id_tile = min(id_tile), X = mean(X), Y = mean(Y)), by = .(mukey)] #remove repeated mukeys in same tile

# Add the id_tile to make folders based on it. If a mukey is in more than 1 tile, it will be located in the lower id_tile
grid10_horizons_v3_dt <- merge(info, grid10_horizons_v2_dt, by = 'mukey')
setcolorder(grid10_horizons_v3_dt, c('id_tile','mukey', 'X', 'Y'))
grid10_horizons_v3_dt[,.(CNCov = unique(CNCov)), by = .(mukey)][,.N, by = .(mukey)][N>1] #has to be empty
grid10_horizons_v3_dt[,.(Salb = unique(Salb)), by = .(mukey)][,.N, by = .(mukey)][N>1] #has to be empty
grid10_horizons_v3_dt[,Salb := round(Salb, 2)]
grid10_horizons_v3_dt[,CNCov := round(CNCov, 2)]
saveRDS(grid10_horizons_v3_dt, "./vr_value/Data/Grid/grid10_horizons_v3_dt.rds")
grid10_horizons_v3_dt <- readRDS("./vr_value/Data/Grid/grid10_horizons_v3_dt.rds")


#-----------------------------------------------------------------
# Step 4 compile the Toolbox
source('./vr_value/Data/APssurgo_master/R/compile_toolbox_v5.R')

compile_toolbox(data_soils = grid10_horizons_v3_dt,
                 badge_name = 'Soils_vr_value',
                 path = "./vr_value/Data/APssurgo_master/APSIM_soils/",
                 crops = tolower(c("Maize","Soybean")))




