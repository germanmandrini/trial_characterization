######################################
# Parallelized Simulations
####################################### 
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

library(stringr)
library(data.table)
# install.packages('XML','~/Rlibs')"

#Get the computer where this is running

if(FALSE){

  server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
  cpsc <-ifelse(Sys.info()["nodename"] == "CPSC-P10E53323", TRUE, FALSE)
  cluster <- str_detect(string = Sys.info()["nodename"], pattern = 'campuscluster')
  print(Sys.info()["nodename"])
  
  #Set the wd
  if(server){
    setwd('~')
  }else if(cpsc){
    setwd('C:/Users/germanm2/Box Sync/My_Documents')
    codes_folder <-'C:/Users/germanm2/Documents'
  }else{
    setwd('/projects/aces/germanm2/')
    cluster <- TRUE	
    codes_folder <- '/projects/aces/germanm2'
  }

}
#===================================
# prepare clusters
#===================================

# no_cores <- ifelse(detectCores() == 8, 6, 32)
# 
# cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized  
#===================================


make_yearly_summary <- function(daily_dt){
  # file_n =  "S:/Bioinformatics Lab/germanm2/trial_characterization_cluster/yc_output/324_680866.rds"
  # file_n <- files_daily[1]
  # file_n <- "S:/Bioinformatics Lab/germanm2/vr_value_v2_cluster/yc_output_1/1367_159876.rds"
  
  # file_n <- "S:/Bioinformatics Lab/germanm2/trial_characterization_cluster/yc_output/1060_173466.rds"
  #--------------------------
  # preparation
  #--------------------------
  
  #--- load libraries ---#
  library(data.table)
  library(stringr)
  
  #=====================================================================================================#
  # Set the periods
  
  #Soybean ------------------------------------------------------------------------------------
  daily_dt[,.N, crop]
  daily_dt[id_trial == 1,.(stage = mean(stage), 
                           .N,
                           day = mean(day)), by = .(stagename  )]
  
  
  daily_dt[crop == 'soybean' & (stagename == 'out' & day < 180), period := 0]
  daily_dt[crop == 'soybean' & (stagename == 'sowing' | stagename == 'germination'| stagename == 'emergence' |stagename == 'end_of_juvenile'), period := 1]
  daily_dt[crop == 'soybean' & (stagename == 'floral_initiation'), period := 2]
  daily_dt[crop == 'soybean' & (stagename == 'flowering'), period := 3]
  daily_dt[crop == 'soybean' & (stagename == 'start_grain_fill'& stage < 7.5), period := 4]
  daily_dt[crop == 'soybean' & (stagename == 'start_grain_fill'& stage >= 7.5), period := 5]                                
  daily_dt[crop == 'soybean' & (stagename == 'end_grain_fill'| stagename == 'maturity'), period := 5]
  daily_dt[crop == 'soybean' & (stagename == 'out' & day >= 180), period := 6]
  daily_dt[crop == 'soybean' & is.na(period)]
  
  
  daily_dt[id_trial == 1,.(stage = mean(stage), 
                           .N,
                           day = mean(day)), by = .(period )]
  
  #Maize ------------------------------------------------------------------------------------
  daily_dt[id_trial == 2,.(stage = mean(stage), 
                           .N,
                           day = mean(day)), by = .(stagename  )]
  daily_dt[crop == 'maize' & (stagename == 'nocrop' & day < 180)]
  daily_dt[crop == 'maize' & (stagename == 'nocrop' & day < 180), period := 0]
  daily_dt[crop == 'maize' & (stagename == 'sowing' | stagename == 'germination'| stagename == 'emergence'), period := 1]
  
  daily_dt[crop == 'maize' & (stagename == 'end_of_juvenile'), period := 2]
  daily_dt[crop == 'maize' & (stagename == 'floral_initiation'& stage < 5.5), period := 2]
  daily_dt[crop == 'maize' & (stagename == 'floral_initiation'& stage >= 5.5), period := 3]
  
  daily_dt[crop == 'maize' & (stagename == 'flag_leaf' | stagename == 'flowering'), period := 3]
  daily_dt[crop == 'maize' & (stagename == 'start_grain_fill'& stage < 8.6), period := 4]
  daily_dt[crop == 'maize' & (stagename == 'end_grain_fill' | stagename == 'maturity' | stagename == 'end_crop'), period := 5]
  daily_dt[crop == 'maize' & (stagename == 'start_grain_fill'& stage >= 8.6), period := 5]
  
  daily_dt[crop == 'maize' & (stagename == 'nocrop' & day >= 180), period := 6]
  daily_dt[crop == 'maize' & is.na(period)]
  
  periods_code_dt <- data.table(period = 0:6,
                                        period_name = c('fallow_initial', 'veg_early', 'veg_late', 
                                                        'flowering', 'grainf_early', 'grainf_late', 'fallow_end'))
  
  data.table::fwrite(periods_code_dt, './trial_characterization_box/Data/output/periods_code.csv')
  
  
  daily_dt[id_trial == 2,.(stage = mean(stage), 
                           .N,
                           day = mean(day)), by = .(period )]
  
  #--------------------------
  #Aggregate by period
  
  period_dt <- daily_dt[,.(rain = round(sum(Rain),1),
                             radn = round(mean(Radn),2),
                             MaxT = round(mean(MaxT),2),
                             MinT = round(mean(MinT),2),
                             swdef_expan = round(mean(swdef_expan),2),
                             period_start_doy = min(day), 
                             esw = round(mean(sw_dep) - mean(ll15_dep),2)), by = .(id_trial, period)]
  ggplot(period_dt)+
    geom_point(aes(x = esw, y = swdef_expan)) +
    facet_free(.~period)
  
  
  period_wide_dt <- dcast(period_dt, id_trial ~ period, value.var = c('rain', 'radn', 'MaxT', 'MinT', 'swdef_expan', 'period_start_doy'))
  
  
  #--------------------------
  # Get some year variables (are not by period)
  yearly_dt <- daily_dt[,.(yield_sim = round(max(Y, na.rm = TRUE)/0.85,0),
                           whc = round(max(dul_dep) - max(ll15_dep),2)), by = id_trial]
  
  #--------------------------
  # Soil information
  horizons_dt <- readRDS("./trial_characterization_box/Data/rds_files/horizons_dt.rds") %>% 
    .[bottom <= 20]
  
  horizons_dt2 <- horizons_dt[,.( sand = round(mean(sand),2), 
                          clay = round(mean(clay),2),
                          om = round(mean(om),2),
                          ph = round(mean(ph),2)), by = id_loc]
  
  
  
  final_dt <- merge(trials_dt[,.(Site, Planting, Crop, state,  region, X, Y, id_trial, id_loc, year)], yearly_dt, by = 'id_trial', all.x = T)
  final_dt <- merge(final_dt, horizons_dt2, by = 'id_loc', all.x = T)
  final_dt <- merge(final_dt, period_wide_dt, by = 'id_trial', all.x = T)
  final_dt[,id_loc := NULL]
  final_dt[year == 2020]

  ggplot(final_dt)+
    geom_point(aes(x = swdef_expan_3, y = yield_sim)) +
    facet_free(.~Crop)
  
    return(final_dt)
}

#----------------------------------------------------------------------------

daily_dt <- readRDS('./trial_characterization_box/Data/rds_files/apsim_output_daily.rds')

c(1:127)[!1:127 %in% (daily_dt$id_trial %>% unique())]

caracterization_dt <- make_yearly_summary(daily_dt)

saveRDS(caracterization_dt, './trial_characterization_box/Data/rds_files/caracterization_dt.rds')
data.table::fwrite(caracterization_dt, './trial_characterization_box/Data/output/caracterization.csv')


ggplot(caracterization_dt)+
  geom_point(aes(x = swdef_expan_3, y = Yield)) +
  facet_free(.~Crop)
