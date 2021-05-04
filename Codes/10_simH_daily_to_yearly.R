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
  
  
  daily_dt[stagename == 'out' & day < 180, period := 0]
  daily_dt[stagename == 'sowing' | stagename == 'germination'| stagename == 'emergence', period := 1]
  daily_dt[stagename == 'end_of_juvenile' , period := 2]
  daily_dt[stagename == 'floral_initiation' , period := 3]
  daily_dt[stagename == 'flowering' , period := 4]
  daily_dt[stagename == 'start_grain_fill'| stagename == 'end_grain_fill'| stagename == 'maturity' , period := 5]
  daily_dt[stagename == 'out' & day >= 180, period := 6]
  daily_dt[is.na(period)]
  
  data.table(period = 0:6,
             period_name = c('fallow_initial', 'veg_early', 'veg_late', 
                             'floral_initiation', 'flowering', 'grain_fill', 'fallow_end'))
  
  daily_dt[id_trial == 1,.(stage = mean(stage), 
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
  yearly_dt <- daily_dt[,.(yield = round(max(Y, na.rm = TRUE)/0.85,0),
                           whc = round(max(dul_dep) - max(ll15_dep),2)), by = id_trial]

  
  final_dt <- merge(yearly_dt, period_wide_dt, by = 'id_trial')
  
  ggplot(final_dt)+
    geom_point(aes(x = swdef_expan_4, y = yield ))
  
  
    return(final_dt)
}

#----------------------------------------------------------------------------

final_dt <- make_yearly_summary(daily_dt)

