######################################
# Parallelized Simulations
######################################

#===================================
# prepare clusters
#===================================

no_cores <- detectCores() * 7/8

cl <- parallel::makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================
#i =   1

apsim_create_files <- function(i){
  # i = 57
  #--------------------------
  # preparation
  #--------------------------
  #--- load libraries ---#
  library(xml2)
  library(data.table)
  library(dplyr)

  trials_tmp <- trials_dt[i]
  #--- load the base apsim file ---# 
  base_doc <- xml2::read_xml("./trial_characterization_box/Data/apsim_files/trial_crct.apsim")
  
  #Clean the plots
  # for(x in xml2::xml_find_all(base_doc, '//Graph')[2:10]){xml2::xml_remove(x)}
  
  #--- edit the met directory ---#
  folder_name <- paste(directory, '/met_files',sep = '')
  
  met_dir <- paste(directory, '/met_files/loc_',trials_tmp$id_loc,'.met', sep = '')
  met_dir <- gsub("/", "\\", met_dir, fixed=TRUE)
  
  node <-  xml_find_all(base_doc,'//metfile/filename')
  xml_text(node) <- met_dir
  
  #--------------------------
  # PLANTING DATE
  #--------------------------
  planting_start <- paste(trials_tmp$day, tolower(trials_tmp$month), trials_tmp$year, sep = '-')
  
  x <- xml_find_all(base_doc, ".//manager/ui/date")
  xml_text(x) <- as.character(planting_start)
  

  #--------------------------
  # CROP AND CULTIVAR 
  #--------------------------
  x <- xml_find_all(base_doc, ".//manager/ui/crop")
  xml_text(x) <- as.character(tolower(trials_tmp$Crop))
  
  
  cultivar <- ifelse(trials_tmp$Crop == 'soybean', 
                     paste0('MG_', trials_tmp$Genetics), 
                     paste0('B_', trials_tmp$Genetics))
  x <- xml_find_all(base_doc, ".//manager/ui/cultivar")
  xml_text(x) <- as.character(cultivar)
  
  
  #--------------------------
  # PLANT POPULATION
  #--------------------------
  # if(instructions_tmp$batch >= 54 ){
  #   plant_population <- c('7', '8.5', '9')[instructions_tmp$region]
  # }
  
  plant_population <- ifelse(trials_tmp$Crop == 'soybean', '30', '8')
  
  x <- xml_find_all(base_doc, ".//manager/ui/density")
  xml_text(x) <- as.character(plant_population)
  
  #--------------------------
  # CLOCK
  #--------------------------
  date_start <-  paste('1', '1', trials_tmp$year, sep = '/')
  date_end <-paste('31','12', trials_tmp$year, sep = '/')
  
  node <- xml_find_all(base_doc,'//clock/start_date')
  xml_text(node) <- date_start
  
  node <-  xml_find_all(base_doc,'//clock/end_date')
  xml_text(node) <- date_end
  
  #--------------------------
  # ROTATION
  #--------------------------
  
  # remove_this_crop <- ifelse(trials_tmp$Crop == 'soybean', 'maize', 'soybean')
  # node <- xml_find_all(base_doc,paste0('//', remove_this_crop))
  # xml2::xml_remove(node)
  
  # if(trials_tmp$Crop == 'soybean'){
  #   crop_seq <- c('soybean', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil',  'nil',  'nil')
  # }else{  
  #   crop_seq <- c('maize', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil',  'nil',  'nil')
  # }
  #   
  # for(crop_n in 1:11){
  # # crop_n = 1
  #   node <- xml_find_all(base_doc, paste0('//manager/ui/crop', crop_n))
  #   xml_text(node) <- crop_seq[crop_n]
  # }
  
  #--------------------------
  # SOIL
  #--------------------------
  #--- extract soil data from the soil data base ---#
  # soils_database <- xmlParse('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/Soils_DIFM_bysoil.soils')
  soils_database <- xml2::read_xml(paste(directory, 'trials_characterization.soils', sep = '/'))
  
  mukey_n = soils_dt[soils_dt$id_loc == trials_tmp$id_loc]$mukey 
  
  soil_name2 <- paste('.//Soil[@name="', tolower(mukey_n), '"]', sep = '')
  
  #--- replace soil ---#
  soil_temp2 <- xml_find_all(soils_database, soil_name2)
  base_doc_soil <- xml_find_all(base_doc, "//Soil")
  
  xml_replace(base_doc_soil, soil_temp2, .copy = TRUE)
  
  rm(soils_database, soil_temp2,base_doc_soil)
  
  #-----------------------------------------------------------------------------------------------
  # 1 - Insert the swim module
  # if(instructions_tmp$water == 'swim'){
  #   swim_file <- xml2::read_xml("./trial_characterization_box/Data/apsim_files/LongTermAssessmentsTileDrainage_ger.apsim")
  #   
  #   node_swim <- xml_find_all(swim_file,'//Swim')
  #   
  #   node_swat <- xml_find_all(base_doc,'//SoilWater')
  #   xml_replace(node_swat, node_swim, .copy = TRUE)
  #   rm(swim_file, node_swim,node_swat)
  # }#end if swim
  #-----------------------------------------------------------------------------------------------
  # 2 - Add water_table to swat
  # if(FALSE & !(is.na(horizons_dt2$watertable[1]))){
  #   watertable_n <- round(horizons_dt2$watertable[1])*10
  #   if(watertable_n < 1500){watertable_n = 1500}
  #   node <- xml_find_all(base_doc,'//manager[@name="Empty manager"]/script')[2]
  #   empty_manager <- xml_text(node)
  #   empty_manager2 <- gsub(pattern = '!water_table = 1000\n\n\n\nstart_of_day', replacement = paste0('water_table = ', watertable_n), empty_manager)
  #   xml_text(node) <- empty_manager2
  # }#end if swat
  #-----------------------------------------------------------------------------------------------
  # 3 - Update the Initial Conditions
  # "C:/Users/germanm2/Documents/trial_characterization_git/Codes/simE_update_ic.R"
  # "./trial_characterization_git/Codes/simE_update_ic.R"
  # source(paste0(codes_folder, '/trial_characterization_git/Codes/simE_update_ic.R')) #simplified version
  # #The initial residue assumes an alternation. Can be improved for account for other types of rotations
  # base_doc <- update_ic(base_doc, instructions_tmp)

  #--- Set the rate for the stab period ---#
  # x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_stab")
  # xml_text(x) <- "150"
  # 
  # #--- Set the starter rate ---#
  # x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_sow")
  # xml_text(x) <- "0"
  # 
  #--- CREATE A FOLDER TO SAVE FILES ---#
  sim_name <- paste('trial', trials_tmp$id_trial, trials_tmp$Crop, sep = '_')
  folder_name <- paste0(directory, '/', sim_name)
  
  if(file.exists(folder_name)){unlink(folder_name ,recursive=TRUE) }
  dir.create(folder_name, recursive = TRUE)
  
  #--- Set the N rate for the trial period ---#
  # N_rates <- 200
  # if(test_small) {N_rates <- 160}
  # if(regional_test){N_rates <- c(0, seq(16, 150, 28), seq(150, 200, 10), seq(212, 270, 28))}
  # # if(regional_test){N_rates <- 160}
  # # N_rates <- seq(0, 300, 25)
  # # N_rates <- c(0,260)
  # # N_rates <- c('Nminus', 'Nrich')
  # # N_rates <- 150
  # for(N_n in N_rates){
    # N_n = N_rates[1]
    # sim_name_n <- paste(sim_name, N_n, sep = '_')
    
    #----------------------------------
    #Change the name of the simulation
    x <-  xml_find_all(base_doc, ".//simulation")
    xml2::xml_attrs(x, 'name')[[1]] <-  c(name = sim_name)
    
    #----------------------------------
    #Change the name of the outputfile
    x <- xml_find_all(base_doc, ".//outputfile/filename")
    xml_text(x) <- as.character(paste(sim_name, '.out', sep = ''))
    
    #----------------------------------
    #Apply the treatment at v5
    # x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_yc")
    # xml_text(x) <- as.character(200)
    
    #--- save as a temporary xml file ---#
    # folder_name_n <- paste(folder_name, N_n, sep = '/')
    # dir.create(folder_name_n, recursive = TRUE)
    filename <- paste0(sim_name, '.apsim')
    
    xml2::write_xml(base_doc, paste(folder_name,'/',filename,sep=''))
}

keep <- c('keep', 'apsim_create_files', 'trials_dt', 'directory', 'codes_folder', 'soils_dt')
# if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # #rm(list = ls()[!ls() %in% keep])

parallel::clusterExport(cl, varlist = keep, envir=environment())

results.list <- parallel::parLapply(cl, 1:nrow(trials_dt), function(x) apsim_create_files(x))

# instructions <- rbindlist(results.list, fill = TRUE)

stopCluster(cl)
