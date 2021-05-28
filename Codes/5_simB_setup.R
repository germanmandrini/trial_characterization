# library(data.table)
library(dplyr)
library(parallel)
library(XML)

#---------------------------------------------------------
# Load soils and horizons and select the ones for this trials
soils_dt <- readRDS("./trial_characterization_box/Data/rds_files/soils_sf.rds") %>% data.table() %>% .[,-'geometry']
# soils_dt <- soils_dt[id_loc == trials_tmp$id_loc]

horizons_dt <- readRDS("./trial_characterization_box/Data/rds_files/horizons_dt.rds") %>%
  .[bottom <= 200] #make soils to only 150 cm
horizons_dt <- horizons_dt[mukey %in% soils_dt$mukey]

weather_dt <- readRDS('./trial_characterization_box/Data/rds_files/weather_dt.rds')
# weather_dt <- weather_dt[id_loc == trials_tmp$id_loc & year == trials_tmp$year]

#---------------------------------------------------------
# Set the folder where the apsim files will be saved
if(server){
  directory <- '/home/germanm2/apsim_temp/trial_characterization'
}else if(cpsc){
  directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/trial_characterization', sep = '')
}else if(cluster){
  directory <- '/projects/aces/germanm2/trial_characterization'
  # directory <- paste('/home/germanm2/scratch/apsim_temp/trial', trial_n, sep = '')
  # directory <- paste('/projects/aces/germanm2/scratch/trial', trial_n, sep = '')
}

unlink(directory ,recursive=TRUE)

#----------------------------------------------------------------------------
# WEATHER FILES
source(paste0(codes_folder, '/trial_characterization_git/Codes/6_simC_make_met_files.R'))
"C:/Users/germanm2/Documents/trial_characterization_git/Codes/6_simC_make_met_files.R"
"./trial_characterization_git/Codes/6_simC_make_met_files.R"

#----------------------------------------------------------------------------
# CREATE SOIL FILES
source(paste0(codes_folder, '/trial_characterization_git/APssurgo_master/R/calc_apsim_variables_onesoil.R'))
'./trial_characterization_git/APssurgo_master/R/calc_apsim_variables_onesoil.R'
"C:/Users/germanm2/Documents/trial_characterization_git/APssurgo_master/R/calc_apsim_variables_onesoil.R"

source(paste0(codes_folder, '/trial_characterization_git/APssurgo_master/R/make_apsoils_toolbox.R'))
'./trial_characterization_git/APssurgo_master/R/make_apsoils_toolbox.R'
"C:/Users/germanm2/Documents/trial_characterization_git/APssurgo_master/R/make_apsoils_toolbox.R"

horizons_dt[is.na(ph), ph := 6] #a few soils didn't have ph and apsim doesn't use it
horizons_dt2 <- calc_apsim_variables(horizons_dt)
# horizons_cell2_dt[bottom >= restriction, XF_maize := 0] #limit the depth of the soil to the restriction

make_apsoils_toolbox(data_soils = horizons_dt2, 
                     badge_name = 'trials_characterization', path = directory, crops = tolower(c("Maize","Soybean")))

#----------------------------------------------------------------------------
# CREATE APSIM FILES
"C:/Users/germanm2/Documents/trial_characterization_git/Codes/7_simD_create_apsim_files.R"
"./trial_characterization_git/Codes/simD_create_apsim_files.R"
source(paste0(codes_folder, '/trial_characterization_git/Codes/7_simD_create_apsim_files.R'))


