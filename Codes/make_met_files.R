#===================================
# prepare clusters
#===================================

# make_met_files_paralell <- function(weather_cell.dt, directory){
#   no_cores <- detectCores() *7/8
#   cl <- makeCluster(no_cores,type='SOCK')
#   
#   #===================================
#   # parallelized simulations 
#   #===================================
# daymet.dt2 <-   weather_trial_dt
  
make_met_files <- function(daymet.dt2, directory = directory){
  # z_n = 'A1'
  packages_need <- c('APSIM','dplyr', 'data.table', 'sf')
  lapply(packages_need, require, character.only = TRUE)
  
  table(daymet.dt2$year) #1999 has to be 365 and 2000 has to be 366

  #------------------------------------------------------------------
  # PREPARE THE FILE
  units_input <- c("()", "()", "(MJ/m^2/day)", "(oC)", "(oC)", "(mm)", "(hours)") 
  setcolorder(daymet.dt2, c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'dayl'))
  daymet.df <- data.frame(daymet.dt2[,-c('lat', 'lon', 'loc_name', 'trial')])
  
  met_file_tmp <- prepareMet(daymet.df, 
                             lat = mean(daymet.dt2$lat), 
                             lon = mean(daymet.dt2$lon),  
                             units = units_input)
  
  table(met_file_tmp@data$year)
  #------------------------------------------------------------------
  # CORRECT TAV AND AMP (they are switched)
  # Amp is obtained by averaging the mean daily temperature of each month over the entire data period resulting in
  # twelve mean temperatures, and then subtracting the minimum of these values from the maximum
  # Tav is obtained by averaging the twelve mean monthly temperatures.
  daymet.dt2[,date := 1:nrow(daymet.dt2)]
  daymet.dt2[,date2 := as.Date(date, origin = paste0(min(daymet.dt2$year)-1, "-12-31"))]
  daymet.dt2[,month := month(date2)]
  daymet.dt2[,meant := (mint+maxt)/2]
  monthly <- daymet.dt2[,.(tmonth = mean(meant)), by = month]
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
  
  fileName_tmp <- paste(folder_name,'/', trial_n, '.met', sep='')
  
  writeMetFile(fileName_tmp, met_file_tmp)
  
  # return(folder_name2)
  
}#end of loc_id_n loop



