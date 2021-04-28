#=====================================================================================================
# STEP 1: Create the data.table with the weathers for each z



#=====================================================================================================
# STEP 2: Create the met files and save them in the simulation folder

#===================================
# prepare clusters
#===================================

# make_met_files_paralell <- function(weather_cell.dt, directory){
# no_cores <- detectCores() *7/8
# cl <- parallel::makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================


make_met_files <- function(weather_dt, directory = directory){
  # z_n = 'A1'
  source(paste0(codes_folder, '/trial_characterization_git/Codes/APSIM_package.R')) #Load the APSIM package (is deprecated)
  packages_need <- c('APSIM','dplyr', 'data.table', 'sf')
  lapply(packages_need, require, character.only = TRUE)
  
 
 
  table(weather_dt$year) #1999 has to be 365 and 2000 has to be 366
  #------------------------------------------------------------------
  # PREPARE THE FILE
  units_input <- c("()", "()", "(MJ/m^2/day)", "(oC)", "(oC)", "(mm)", "(hours)") 
  setcolorder(weather_dt, c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'dayl'))
  daymet.df <- data.frame(weather_dt[,-c('id_loc', 'X', 'Y')])
  
  met_file_tmp <- prepareMet(daymet.df, 
                             lat = mean(weather_dt$Y), 
                             lon = mean(weather_dt$X),  
                             units = units_input)
  
  table(met_file_tmp@data$year)
  #------------------------------------------------------------------
  # CORRECT TAV AND AMP (they are switched)
  # Amp is obtained by averaging the mean daily temperature of each month over the entire data period resulting in
  # twelve mean temperatures, and then subtracting the minimum of these values from the maximum
  # Tav is obtained by averaging the twelve mean monthly temperatures.
  weather_dt[,date := 1:nrow(weather_dt)]
  weather_dt[,date2 := as.Date(date, origin = paste0(min(weather_dt$year)-1, "-12-31"))]
  weather_dt[,month := month(date2)]
  weather_dt[,meant := (mint+maxt)/2]
  monthly <- weather_dt[,.(tmonth = mean(meant)), by = month]
  Amp <- max(monthly$tmonth) - min(monthly$tmonth)
  Tav <- mean(monthly$tmonth)
  
  met_file_tmp@amp <- Amp
  met_file_tmp@tav <- Tav
  
  #------------------------------------------------------------------
  # SAVE THE FILE
  #--- save as a temporary xml file ---#

  # unlink(folder ,recursive=TRUE)
  folder_name <- paste(directory, '/met_files',sep = '')
  
  if(!file.exists(folder_name)){dir.create(folder_name, recursive = TRUE)}
  
  fileName_tmp <- paste(folder_name,'/trial_', trial_n, '.met', sep='')
  
  writeMetFile(fileName_tmp, met_file_tmp)
  
  # return(folder_name2)
  
}#end of loc_id_n loop

make_met_files(weather_dt, directory)





