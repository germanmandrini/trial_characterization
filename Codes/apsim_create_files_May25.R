######################################
# Parallelized Simulations
######################################

#===================================
# prepare clusters
#===================================
# 
# no_cores <- detectCores() * 7/8
# 
# cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================
#i =   10
library(lubridate)

apsim_create_files <- function(trial_n){
  # i = 1
  #--------------------------
	# preparation
	#--------------------------
	#--- load libraries ---#
  # library(xml2)
  # library(data.table)
  # library(dplyr)
  # 
  # instructions_tmp <- instructions[i]
  # sim_name <- paste(instructions_tmp, collapse = '_')
 	#--- load the base apsim file ---# 
 
  base_doc <- xml2::read_xml("./G2F/Data/apsim_files/G2F_v6.apsim")
 
  #Clean the plots
  # for(x in xml2::xml_find_all(base_doc, '//Graph')[2:10]){xml2::xml_remove(x)}

  #--- edit the met module directory ---#
  met_dir <- paste(directory, '/met_files','/', trial_n, '.met', sep='')
  
  met_dir <- gsub("/", "\\", met_dir, fixed=TRUE)
  
  node <-  xml_find_all(base_doc,'//metfile/filename')
  xml_text(node) <- met_dir
  
  #--------------------------
  # CLOCK
  #--------------------------
  year_start <- one_trial_dt$year-1
  date_end <- paste0('31/12/', one_trial_dt$year)
 
  node <- xml_find_all(base_doc,'//clock/start_date')
  xml_text(node) <- paste('01/01/',year_start, sep = '')
  
  node <-  xml_find_all(base_doc,'//clock/end_date')
  xml_text(node) <- date_end
  
  #--------------------------
  # ROTATION
  #--------------------------
  # if(instructions_tmp$type == 'stab'){
  #   if(instructions_tmp$rotation == 'SMM'){ 
  #     crop_seq <- c('maize', 'soybean', 'maize', 'maize', 'soybean', 'maize', 'maize', 'soybean', 'maize', 'maize')
  #     } else { 
  #     crop_seq <- c('maize', 'maize', 'soybean', 'maize', 'maize', 'soybean', 'maize', 'maize', 'soybean', 'maize')}
  # }else{
  #   if(instructions_tmp$rotation == 'SMM'){ 
  #     crop_seq <- c('maize', 'maize', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil')
  #   } else { 
  #    crop_seq <- c('soybean', 'maize', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil')
  # }
    
  # for(crop_n in 1:10){
  #  # crop_n = 1
  #  node <- xml_find_all(base_doc, paste0('//manager/ui/crop', crop_n))
  #  xml_text(node) <- crop_seq[crop_n]
  #  }
  
  #--- extract soil data from the soil data base ---#
  # soils_database <- xmlParse('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/Soils_DIFM_bysoil.soils')
  soils_database <- xml2::read_xml(paste(directory, 'soils_G2F.soils', sep = '/'))
  
  soil_name2 <- paste('.//Soil[@name="', mukey_n, '"]', sep = '')
  
  #--- replace soil ---#
  soil_temp2 <- xml_find_all(soils_database, soil_name2)
  base_doc_soil <- xml_find_all(base_doc, "//Soil")

  xml_replace(base_doc_soil, soil_temp2, .copy = TRUE)
  
  rm(soils_database, soil_temp2,base_doc_soil)
  
  # if(instructions_tmp$type == 'YC'){
  #   source('./G2F/Codes/update_ic_May25.R')
  #   base_doc <- update_ic(base_doc, instructions_tmp)
  # }
  
  #--- Set the rate for the stab period ---#
  # x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_stab")
  # xml_text(x) <- "150"
  
  #--- edit sowing date ---#
  #year.fert <- 1
  
  dates_sowing_soy <-  as.Date(one_trial_dt$planting_date1, format = "%m/%d/%Y")
  dates_sowing_corn <-  as.Date(one_trial_dt$planting_date2, format = "%m/%d/%Y")

  # dates_sowing_soy <- ymd(dates_sowing_corn) - years(1)
  dates_sowing_soy2 <- strftime(dates_sowing_soy, format = "%d-%b-%Y")
  dates_sowing_corn2 <- strftime(dates_sowing_corn, format = "%d-%b-%Y")

  
  #--- edit sowing date---#
  node <- xml_find_all(base_doc, "//manager/ui/date")[1]
  xml_text(node) <- dates_sowing_soy2
    
  node <- xml_find_all(base_doc, "//manager/ui/date")[2]
  xml_text(node) <- dates_sowing_corn2
  
  #--- edit variety ---#
  node <- xml_find_all(base_doc, "//manager/ui/cultivar")[1]
  xml_text(node) <- paste0('MG_', one_trial_dt$gm)
  
  #--- edit hybrid ---#
  node <- xml_find_all(base_doc, "//manager/ui/cultivar")[2]
  xml_text(node) <- paste0('B_', one_trial_dt$rm)
  
  #--- edit irrigation ---#
  node <- xml_find_all(base_doc, "//irrigation/automatic_irrigation")
  xml_text(node) <-   ifelse(!is.na(one_trial_dt$Irrigation),'on', 'off')
  
  #--- CREATE A FOLDER TO SAVE FILES ---#
  folder_name <- directory
  
  # if(file.exists(folder_name)){unlink(folder_name ,recursive=TRUE) }
  # dir.create(folder_name, recursive = TRUE)
  
  #--- Set the rate for the YC period ---#
  # if(instructions_tmp$type == 'YC'){
  #   N_rates <- seq(0, 320, 10)
  #   for(N_n in N_rates){
  #     # N_n = 30
  #     sim_name_n <- paste(sim_name, N_n, sep = '_')
  # 
  #     #----------------------------------
  #     #Change the name of the simulation
  #     x <-  xml_find_all(base_doc, ".//simulation")
  #     xml2::xml_attrs(x, 'name')[[1]] <-  c(name = sim_name_n)
  # 
  #     #----------------------------------
  #     #Change the name of the outputfile
  #     x <- xml_find_all(base_doc, ".//outputfile/filename")
  #     xml_text(x) <- as.character(paste(sim_name_n, '.out', sep = ''))
  # 
  #     #----------------------------------
  #     #Change the rate
  #     x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_yc")
  #     xml_text(x) <- as.character(N_n)
  # 
  # 
  #     #--- save as a temporary xml file ---#
  #     folder_name_n <- paste(folder_name, N_n, sep = '/')
  #     dir.create(folder_name_n, recursive = TRUE)
  #     filename <- paste0(sim_name_n, '.apsim')
  # 
  #     xml2::write_xml(base_doc, paste(folder_name_n,'/',filename,sep=''))
  #   }
  # }else{
  #   #----------------------------------
  #   #Change the name of the simulation
  #   x <-  xml_find_all(base_doc, ".//simulation")
  #   xml2::xml_attrs(x, 'name')[[1]] <-  c(name = sim_name)
  # 
  #   #----------------------------------
  #   #Change the name of the outputfile
  #   x <- xml_find_all(base_doc, ".//outputfile/filename")
  #   xml_text(x) <- as.character(paste(sim_name, '.out', sep = ''))

    #--- save as a temporary xml file ---#
    filename <- paste0(directory, '/', trial_n, '.apsim')
    xml2::write_xml(base_doc, filename)
# 
#   }
  # apsimExe <- 'C:/Program Files (x86)/APSIM710-r4171/Model/Apsim.exe'
  # simulation <- apsimr::apsim(exe=apsimExe, wd=folder_name, files=filename)
  # simulation_dt <- data.table(simulation)
  # simulation <- apsim(exe=apsimExe, wd=folder_name, files=filename)
  
  # apsimWd <- folder_name2
  # apsimExe <- '/opt/apsim_dev/trunk/Model/ApsimModel.sh'
  # 
  # simulation <- suppressWarnings(apsimr::apsim(exe=apsimExe, wd=apsimWd, files=filename))
  # 
  
  # instructions_tmp[,dir_path := folder_name]
  # initial_conditions[mukey == instructions_tmp$mukey & z == instructions_tmp$z & year == 2010, Y_dry]
	#return(one_treat.tmp)
  return(filename)
}


# keep <- c('keep', 'apsim_create_files', 'instructions', 'directory')
# # if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # # #rm(list = ls()[!ls() %in% keep])
# 
# clusterExport(cl, varlist = keep, envir=environment())
# 
# 
# results.list <- parallel::parLapply(cl, 1:nrow(instructions), function(x) apsim_create_files(x))
# 
# instructions <- rbindlist(results.list, fill = TRUE)
# 
# stopCluster(cl)


# apsim_create_files(1)
# results.list <- list()
# 
# for(i in 1:nrow(instructions)){
#   # print(i)
#   results.list[[i]] <- apsim_create_files(i)
# }

# instructions <- rbindlist(results.list, fill = TRUE)

# 

# Delete temporary folders created during simulations
# for(N_n in N_seq){
#   folder.name <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/EONR_irr/loc_id_',LOC_id_n ,'_z_', z_n, '_N_',N_n, sep = '')
#   unlink(folder.name ,recursive=TRUE)}

       

