# library(data.table)
library(dplyr)
library(parallel)
library(XML)

#---------------------------------------------------------
# Load soils and horizons and select the ones for this trials
soils_dt <- readRDS("./trial_characterization_box/Data/rds_files/soils_sf.rds") %>% data.table() %>% .[,-'geometry']
soils_dt <- soils_dt[id_loc == trials_tmp$id_loc]

horizons_dt <- readRDS("./trial_characterization_box/Data/rds_files/horizons_dt.rds") %>%
  .[bottom <= 200] #make soils to only 150 cm
horizons_dt <- horizons_dt[mukey %in% soils_dt$mukey]

weather_dt <- readRDS('./trial_characterization_box/Data/rds_files/weather_dt.rds')
weather_dt <- weather_dt[id_loc == trials_tmp$id_loc & year == trials_tmp$year]

#---------------------------------------------------------
# Set the folder where the apsim files will be saved
if(server){
  directory <- paste('/home/germanm2/apsim_temp/trial_characterization/trial', trial_n, sep = '')
}else if(cpsc){
  directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/trial_characterization/trial', trial_n, sep = '')
}else if(cluster){
  directory <- paste('/projects/aces/germanm2/trial_characterization/trial', trial_n, sep = '')
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

horizons_dt2 <- cbind(horizons_dt2,trials_tmp[,.(X,Y)])
make_apsoils_toolbox(data_soils = horizons_dt2, 
                     badge_name = 'trials_characterization', path = directory, crops = tolower(c("Maize","Soybean")))

#----------------------------------------------------------------------------
# CREATE THE INSTRUCTIONS FOR THE STABILIZATION PERIOD 
# is_even <- function(x) x %% 2 == 0
# z_seq <- unique(weather_cell.dt$z)
# z_even = z_seq[is_even(z_seq)]
# z_odd = z_seq[!is_even(z_seq)]
# 
# if(any(one_cell_dt$id_field %in% c(1,3))){
#   instructions1 <- data.table(id_10 = trial_n,
#                               region = one_cell_dt$region[1],
#                               expand.grid(z = z_odd,
#                                           mukey = sort(unique(one_cell_dt[id_field %in% c(1,3)]$mukey)),
#                                           stringsAsFactors = FALSE),
#                               stringsAsFactors = FALSE) 
# }else{instructions1 <- data.table()}
# 
# 
# if(any(one_cell_dt$id_field %in% c(2,4))){
#   instructions2 <- data.table(id_10 = trial_n,
#                               region = one_cell_dt$region[1],
#                               expand.grid(z = z_even,
#                                           mukey = sort(unique(one_cell_dt[id_field %in% c(2,4)]$mukey)),
#                                           stringsAsFactors = FALSE),
#                               stringsAsFactors = FALSE) 
# }else{instructions2 <- data.table()}
# 
# instructions <- rbind(instructions1, instructions2) %>% setcolorder(c('id_10',  'mukey', 'z'))
# instructions <- merge(instructions, horizons_cell2_dt[, .(watertable = mean(watertable)), by = mukey], by = 'mukey')
# instructions[,batch := batch_n]
# instructions[,water := water_n]
# 
# #---------------------------------------------------------------
# # N by region, and same by field and z combination
# set.seed(1)
# z_count <- length(unique(instructions$z))
# n_target_vector <- list(sample(1:60, z_count, replace = T), #South
#                         sample(1:60, z_count, replace = T),  #Central
#                         sample(1:60, z_count, replace = T))[[region_n]] #North
# 
# # n_target_vector <- list(sample(c(1:10,50:60), z_count, replace = T), #South
# #                         sample(c(1:20,40:60), z_count, replace = T),  #Central
# #                         sample(c(1:20,50:66), z_count, replace = T))[[region_n]] #North
# 
# n_target_dt <- data.table(z = unique(instructions$z),
#                           n_target= n_target_vector)
# instructions <- merge(instructions, n_target_dt, by = 'z')
#---------------------------------------------------------------
# if(regional_test) {instructions <- instructions[z %in% c(2,3,6,7,13,14,15,16,24,25,28,29)]}
# if(test_small) {instructions <- instructions[1,]}
# if(FALSE) {instructions <- instructions[z==23,]}
# print(instructions )
"C:/Users/germanm2/Documents/trial_characterization_git/Codes/7_simD_create_apsim_files.R"
"./trial_characterization_git/Codes/simD_create_apsim_files.R"
source(paste0(codes_folder, '/trial_characterization_git/Codes/7_simD_create_apsim_files.R'))


