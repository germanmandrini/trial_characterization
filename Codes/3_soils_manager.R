rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd('~')#Server
# codes_folder <-'~' #Server

source('./Codes_useful/R.libraries.R')

# ---------------------------------------------------------------------------------------------
# Load libraries and functions #################################################
library(maps)
library(maptools)

# Load the locs
locs_sf <- readRDS('./trial_characterization_box/Data/rds_files/locs_sf.rds')

#---------------------------------------------------------------
# Step 1 Get the soils for each field
source(paste0(codes_folder, '/trial_characterization_git/APssurgo_master/R/get_soils_parallel.R'))
'C:/Users/germanm2/Documents/trial_characterization_git/APssurgo_master/R/get_soils_parallel.R'
soils_sf <- readRDS('./trial_characterization_box/Data/rds_files/soils_sf.rds')


#---------------------------------------------------------------  
# Step 2 get the horizons information
source(paste0(codes_folder, '/trial_characterization_git/APssurgo_master/R/get_horizons_parallel.R'))
"C:/Users/germanm2/Documents/trial_characterization_git/APssurgo_master/R/get_horizons_parallel.R"

horizons_dt <- readRDS("./trial_characterization_box/Data/rds_files/horizons_dt.rds")

# Clean the soils not available in SSURGO

all(soils_sf$mukey %in% horizons_dt$mukey)





