# Load libraries and functions #################################################
#
# install.packages(c("shiny","leaflet","shinythemes","FedData","maps",
#                   "maptools","rgdal","raster","dplyr","ggplot2","XML",
#                   "Hmisc","lubridate"))
# install.packages('shiny')
# library(shiny)
# install.packages('leaflet')
# library(leaflet)
# install.packages('shinythemes')
# library(shinythemes)
# library(FedData)
library(maps)
library(maptools)
# library(rgdal)
# library(raster)
# library(dplyr)
# library(ggplot2)
# library(XML)
# library(Hmisc)
# library(lubridate)

setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

files <- list.files('./Trial_crct_DIFM/Data/cleaned_and_merged_corrected', full.names = T)

#setwd('./Trial_crct_DIFM/Data/APssurgo_master')
#source("R/downloadSSURGO.R")
#


source("./Trial_crct_DIFM/Data/APssurgo_master/R/get_soils.R")
source("./Trial_crct_DIFM/Data/APssurgo_master/R/calc_avg_soil.R")
source("./Trial_crct_DIFM/Data/APssurgo_master/R/compile_toolbox_v5.R")



# source("R/moldSSURGO_gm.R")




all_soils <- data.table()

for(file_n in files){
  # file_n <- files[6]
  us_states4326.sf <- st_transform(us_states, 4326) %>% dplyr::select(NAME, REGION)

  farm <- strsplit(strsplit(file_n, '/')[[1]][5], '_')[[1]][1]
  field <- strsplit(strsplit(file_n, '/')[[1]][5], '_')[[1]][2]
  #year <- str_extract(file_n, '[0-9]{4}')

  app_map <- read_sf(file_n)  %>%
    st_transform(crs = 4326)

  tm_shape(us_states4326.sf) + tm_borders() +
    tm_shape(app_map) + tm_dots(col  = 'red', size = 0.3)+
    tm_layout(main.title = 'Location')

  tm_shape(app_map) + tm_polygons('ntotal_kgha')+
    tm_layout(main.title = 'Trial')

  #ERROR HANDLING
  possibleError <- tryCatch({

    x <- get_soils(app_map, max_soil_depth = 200, extend_to_limit = TRUE)

    # Step 2 Calculate new variables

    x2 <- calc_avg_soil(x, soilLayer_breaks = c(20,40,60,150,200))

  }, error=function(e) e)

  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){

      x3 <- data.table(farm, field, st_coordinates(st_centroid(app_map[1,])), x2)

      all_soils <- rbind(all_soils, x3)
      # Step 4 Create XML FILE
      # incProgress(1/5, detail = "Compiling soil XLM file")
  }#end of the real work
}#end of files_n loop

saveRDS(all_soils, './Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/all_soils.rds')
all_soils <- readRDS('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/all_soils.rds')

all_soils[,.(CNCov = unique(CNCov)), by = .(farm, name)][,.N, by = .(farm, name)]
all_soils[,.(Salb = unique(Salb)), by = .(farm, name)][,.N, by = .(farm, name)]

all_soils[,Salb := round(Salb, 2)]
all_soils[,CNCov := round(CNCov, 2)]


# all_soils <- all_soils[SiteName == 'Gingerich']
# all_soils[, (colnames(all_soils)) := lapply(.SD, as.character),]
# sapply(all_soils,class)


compile_toolbox(data_soils = all_soils,
             badge_name = 'Soils_DIFM_bysoil',
             path = "./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/",
             crops = tolower(c("Maize","Soybean")))




