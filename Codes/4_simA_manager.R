rm(list=ls())

library(stringr)
library(data.table)

#Get the computer where this is running
server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
cpsc <-ifelse(Sys.info()["nodename"] == "CPSC-P10E53323", TRUE, FALSE)
cluster <- str_detect(string = Sys.info()["nodename"], pattern = 'campuscluster')
print(Sys.info()["nodename"])

#Set the wd
if(server){
  setwd('~')
  codes_folder <- getwd()
}else if(cpsc){
  setwd('C:/Users/germanm2/Box Sync/My_Documents')
  codes_folder <-'C:/Users/germanm2/Documents'
}

# ---------------------------------------------------------------------------------------------
# Load needed files
trials_dt <- readRDS("./trial_characterization_box/Data/rds_files/trials_sf.rds") %>% 
  data.table() %>% .[,-'geometry']


trial_n = 1

# for(trial_n in 1:nrow(trials_dt)){
  
  trials_tmp <- trials_dt[trial_n]
  
  
  # CREATE ALL FILES
  start1 <- Sys.time()
  "C:/Users/germanm2/Documents/trial_characterization_git/Codes/5_simB_setup.R"
  "./trial_characterization_git/Codes/5_simB_setup.R"
  source(paste0(codes_folder, '/trial_characterization_git/Codes/5_simB_setup.R'))
  # instructions1_rows <- nrow(instructions)
  
  #RUN ALL APSIM FILES
  start2 <- Sys.time()
  "C:/Users/germanm2/Documents/trial_characterization_git/Codes/8_simF_run_files.R"
  "./trial_characterization_git/Codes/8_simF_run_files.R"
  source(paste0(codes_folder, '/trial_characterization_git/Codes/8_simF_run_files.R'))
  
  #MERGE ALL THE OUTPUT
  start3 <- Sys.time()
  "C:/Users/germanm2/Documents/trial_characterization_git/Codes/9_simG_merge_results.R"
  "./trial_characterization_git/Codes/9_simG_merge_results.R"
  source(paste0(codes_folder, '/trial_characterization_git/Codes/9_simG_merge_results.R'))
  
  start4 <- Sys.time()
  
  #MAKE YEARLY SUMMARY
  files_daily <- list.files(paste0('./trial_characterization_box/Data/yc_output_', batch_n, '_', water_n), pattern = paste0('^',id10_n, '_'), full.names = T)
  print(files_daily)
  "C:/Users/germanm2/Documents/trial_characterization_git/Codes/10`"
  './trial_characterization_git/Codes/simH_daily_to_yearly.R'
  source(paste0(codes_folder, '/trial_characterization_git/Codes/simH_daily_to_yearly.R'))
  
  unlink(directory, recursive = TRUE)
  
  start5 <- Sys.time()
  
  time_track_tmp <- data.table(id_10 = id10_n,
                               mukey_n = length(unique(instructions$mukey)),
                               time = start1,
                               inst = instructions1_rows,
                               create = as.numeric(difftime(start2, start1, units = "mins")),
                               run = as.numeric(difftime(start3, start2, units = "mins")),
                               merge_save = as.numeric(difftime(start4, start3, units = "mins")),
                               yearly_summary = as.numeric(difftime(start5, start4, units = "mins")),
                               cell = as.numeric(difftime(start5, start1, units = "mins")))
  print(time_track_tmp)
  
  folder_name <- paste0('./trial_characterization_box/Data/time_track_', batch_n)
  if(!file.exists(folder_name)){dir.create(folder_name, recursive = TRUE)}
  saveRDS(time_track_tmp, paste0(folder_name,'/time_track_',id10_n,'.rds'))
  
  
  
}#end trial_n loop
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


# all_locs_weather_dt <- readRDS('./trial_characterization_box/Data/met_files/all_locs_weather_dt.rds')

source(paste0(codes_folder, '/trial_characterization_git/APssurgo_master/R/calc_apsim_variables_onesoil.R'))


"C:/Users/germanm2/Documents/trial_characterization_git/APssurgo_master/R/calc_apsim_variables_onesoil.R"


source('./trial_characterization_box/Data/APssurgo_master/R/make_apsoils_toolbox.R')
source('./G2F/Codes/make_met_files.R')
source('./G2F/Codes/apsim_merge_data.R')
source('./G2F/Codes/apsim_create_files_May25.R')
# yc_yearly_dt <- readRDS("./trial_characterization_box/Data/files_rds/yc_yearly_dt.rds")
# missing_ids <- yc_yearly_dt[,.N, by = .(id_10, mukey)][N != 1650]
# rm(yc_yearly_dt)
# id_10_seq <- unique(missing_ids$id_10)


# all_soils <- readRDS('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/all_soils.rds')
# unlink("/home/germanm2/apsim_temp/trial_crct" ,recursive=TRUE)

#----------------------------------------------------------------------------
# MAKE SOME MAPS AND CHOOSE THE TILES
# grid10_tiles_sf <- readRDS("./trial_characterization_box/Data/Grid/grid10_tiles.sf5.rds") 
# tm_shape(grid10_tiles_sf) + tm_polygons("county_name")

#----------------------------------------------------------------------------
# LIST RUNNED FILES
# 
# runned <- list.files('./trial_characterization_box/Data/yc_output/')
# id_10_runned <- unique(as.numeric(unlist(str_extract_all(runned, pattern = '[0-9]+'))))
#           
# id_10_seq <- sort(unique(grid10_soils_sf4$id_10))
# id_10_seq <- id_10_seq[!id_10_seq %in% id_10_runned] %>% .[!id_10_seq %in% problems]
# id_10_seq <- sample(id_10_seq)

# time_track <- data.table()

for(trial_n in fields_dt$trial){
  # trial_n = "DEH1_2015"
  print(trial_n)
  start1 <- Sys.time()
  
  #Get the directory to save the run
  server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
  
  if(server){
    directory <- paste('/home/germanm2/apsim_temp/G2F/cell', id10_n, sep = '')
  }else{
    directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/G2F/',trial_n, sep = '')
  }
  
  unlink(directory ,recursive=TRUE)
  
  one_trial_dt <- fields_dt[trial == trial_n,]
  
  #----------------------------------------------------------------------------
  # WEATHER FILES
  weather_trial_dt <- weather_dt[trial == trial_n,]
  
  make_met_files(weather_trial_dt, directory)
  
  #----------------------------------------------------------------------------
  # INITIAL SOIL FILES (WE WILL UPDATE THEM AFTER STABILIZATION)
  mukey_n <- soils_dt[trial == trial_n,][area_ha == max(area_ha)]$mukey
  
  horizons_trial_dt <- horizons_dt[mukey == mukey_n,]
  
  horizons_trial_dt[is.na(ph), ph := 6] #a few soils didn't have ph and apsim doesn't use it
  horizons_trial_dt2 <- calc_apsim_variables(horizons_trial_dt)
  horizons_trial_dt2 <- cbind(horizons_trial_dt2, one_trial_dt[,.(X,Y)])
  
  
  make_apsoils_toolbox(data_soils = horizons_trial_dt2, badge_name = 'soils_G2F', 
                       path = directory, crops = tolower(c("Maize","Soybean")))
  
  #CREATE ALL APSIM FILES
  apsim_create_files(trial_n)
}

#RUN ALL APSIM FILES
directory <- dirname(directory)
source('./G2F/Codes/apsim_run_files_apr4.R')

#MERGE ALL THE OUTPUT

apsim_merge_data(directory_files = directory, directory_output= './trial_characterization_box/Data/output')


results_dt <- readRDS('./trial_characterization_box/Data/output/output.rds')

results_yearly_dt <- cbind(results_dt[year != year_trial, .(Yld_soy = max(Y, na.rm = T)), by = trial],
      results_dt[year == year_trial, .(Yld_corn = max(Y, na.rm = T)), by = trial][,-'trial'])


yield_dt <- data.table(openxlsx::read.xlsx('./trial_characterization_box/Data/g2f_databases/phenotipic_data.xlsx', 'yield_loc'))

results_yearly_dt2 <- merge(results_yearly_dt[,.(trial, yld_sim = Yld_corn)], yield_dt[,.(trial, yld_obs = yld_kg_ha)], by = 'trial')

ggplot(data=results_yearly_dt2, aes(x = yld_obs, y = yld_sim)) +
  geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(0, 13000)+ xlim(0, 13000) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  ggtitle('Sequential vs Continuous') +
  theme_bw()+
  geom_text(aes(label = trial), size = 3)
