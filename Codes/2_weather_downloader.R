rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd('~')#Server
# codes_folder <-'~' #Server

source('./Codes_useful/R.libraries.R')
# ---------------------------------------------------------------------------------------------
# Load the locs
locs_sf <- readRDS('./trial_characterization_box/Data/rds_files/locs_sf.rds')


download_weather <- function(loc_n){
  # loc_n = 1
  print(loc_n)
  
  packages_need <- c('APSIM', 'daymetr','dplyr', 'data.table', 'sf')
  lapply(packages_need, require, character.only = TRUE)
  
  one_loc_dt <- data.table(locs_sf[loc_n,])
  
  daymet.daymetr <- download_daymet(site = one_loc_dt$id_loc, 
                        lat = one_loc_dt$Y, lon = one_loc_dt$X, 
                        # start = 1979, end = one_10$year, 
                        internal = TRUE)

  # data.table::fwrite(daymet.daymetr$data, './vr_value/Data/met_files/testdata.csv')
  daymet.dt <- daymet.daymetr$data %>% data.table() %>% 
    .[,.(year, day=yday, radn=srad..W.m.2., maxt=tmax..deg.c., mint=tmin..deg.c., 
         rain=prcp..mm.day., dayl = dayl..s.)]
  #------------------------------------------------------------------
  #CONVERT SOLAR RADIATION
  # Watts/m2 = MJ/m2 * 1000000 / (60 * 60 * 24) https://cliflo-niwa.niwa.co.nz/pls/niwp/wh.do_help?id=ls_rad
  # input = srad	W/m2	Incident shortwave radiation flux density in watts per square meter, 
  # taken as an average over the daylight period of the day. 
  # NOTE: Daily total radiation (MJ/m2/day) can be calculated as follows: 
  # ((srad (W/m2) * dayl (s/day)) / l,000,000) #https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html
  # output = (MJ/m^2/day) #prepareMet documentation
  
  daymet.dt[,radn := radn * dayl/1000000]
  daymet.dt[,dayl := dayl/3600]
  summary(daymet.dt$radn)
  # daymet.dt[,.N, by=year]
  # daymet.dt[,.(pp = sum(rain)), by=year]
  # #head(daymet_tmp.df)
  # # head(daymet.dt)
  #newNames <-c("Date", "maxt", "mint", "rain", "evaporation", "radn", "vp", "Wind", "RH", "SVP") 
 
  #------------------------------------------------------------------
  #CORRECT LEAP YEAR
  # The Daymet calendar is based on a standard calendar year. All Daymet years have 1 - 365 days, including leap years. 
  # For leap years, the Daymet database includes leap day. Values for December 31 are discarded from leap years to maintain a 365-day year.
  # correction_leap.dt <- daymet.dt[year %in% leap_years & day %in% c(59, 60)]
  # correction_leap.dt <- correction_leap.dt[,.(day = mean(day), radn = mean(radn), mint = mean(mint), maxt = mean(maxt), rain = mean(rain), dayl = mean(dayl)), by = year]
  table(daymet.dt$year)
  # daymet.dt2 <- rbind(daymet.dt, correction_leap.dt) %>% .[order(year, day)]
  
  # daymet.dt2[ ,day := NULL]
  # daymet.dt2[ ,day := 1:.N, by = year]
  setcolorder(daymet.dt, c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'dayl'))

  
  leap_years <- seq(1980,2030, by = 4)
  correction_leap.dt <- daymet.dt[year %in% leap_years & day %in% 59:60]
  correction_leap.dt <- correction_leap.dt[,.(day = mean(day),
                                              radn = mean(radn),
                                              maxt = mean(maxt),
                                              mint = mean(mint),
                                              rain = mean(rain),
                                              dayl = mean(dayl)), by = .(year)]
  nrow(correction_leap.dt)
  
  daymet_dt2 <- rbind(daymet.dt, correction_leap.dt) %>% .[order(year, day)]
  daymet_dt2[, day2 := seq_len(.N), by = .(year)]
  daymet_dt2[,day := NULL]
  setnames(daymet_dt2, 'day2', 'day')
  
  daymet_dt2[,.N, by = .(year)][,.(mean(N)), by = year]
  
  daymet_dt3 <- cbind(data.table(one_loc_dt) %>% .[,.(id_loc, X, Y)], daymet_dt2)
  
  return(daymet_dt3)
}


results_list <- lapply(X = 1:nrow(locs_sf), function(x) download_weather(x))

# results_list <- list()
# for(id_10_n in ids_10_seq[1:10]){
#   results_list[[length(results_list)+1]] <- download_weather(id_10_n, grid10_tiles_sf)
# }

weather_dt <-  rbindlist(results_list)

saveRDS(weather_dt, './trial_characterization_box/Data/rds_files/weather_dt.rds')




