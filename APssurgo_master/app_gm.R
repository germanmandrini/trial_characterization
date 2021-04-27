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
library(FedData)
library(maps)
library(maptools)
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(XML)
library(Hmisc)
library(lubridate)
setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

setwd('./Lopt_UR/Data/APssurgo-master')
#source("R/downloadSSURGO.R")
# 


source("R/calcSSURGO.R")
source("R/ssurgo2apsim_gm.R")
source("R/moldSSURGO_gm.R")

# input_all.df <- data.frame(soils.dt,
#                     mySoil_layers = c("20,40,60,150,250"),
#                     myBy_soil = TRUE, #avg across soil types
#                     #myCrops = c("Maize","Soybean"),
#                     myMap_save = FALSE,
#                     stringsAsFactors=FALSE)

us_states4326.sf <- st_transform(us_states, 4326) %>% dplyr::select(NAME, REGION)

SiteName = 'lott_ols_2018'

app_map <- st_read('./boundaries/lott_ols_2018_appmap30and90.shp') %>% 
  st_transform(crs = 4326)

a <- polygon_from_extent(raster::extent(app_map),
                         proj4string="+proj=longlat")


tm_shape(us_states4326.sf) + tm_borders() +
  tm_shape(app_map) + tm_dots(col  = 'red', size = 0.3)+
  tm_layout(main.title = 'Location')

tm_shape(a) + tm_polygons()+
  tm_shape(app_map) + tm_borders(col = 'blue')+
  tm_layout(main.title = 'Trial and extension')

#ERROR HANDLING
possibleError <- tryCatch({
  
  x <- get_ssurgo(template=a, label=SiteName)
  
      # Step 2 Mold data

    x2 <- moldSSURGO_gm(x,soilLayer_breaks = c(20,40,60,150,200), extend_to_limit = TRUE)
    
    # Step 3 Calculate new variables
    #incProgress(1/5, detail = "Calculating new variables")
    
    x3 <- calcSSURGO(x2,input_n$myBy_soil,soil_layers)
    
}, error=function(e) e)

#REAL WORK:if there is no error
if(!inherits(possibleError, "error")){
  
    x3$Average_Soil$coords <- input_n[,c('LOC_ID', 'LAT', 'LONG')]
    
    list_of_soils[[SiteName]] <- x3
    # Step 4 Create XML FILE
    # incProgress(1/5, detail = "Compiling soil XLM file")
}#end of the real work
}#end of row_n loop

saveRDS(list_of_soils, './APSIM_soils/list_of_soils.rds')  
list_of_soils <- readRDS('./Crop_Challenge_gm/rds_files/list_of_soils.rds') 


list_of_soils

LOC_ID <- str_split(names(list_of_soils), '_', simplify = TRUE)[,3]


SSURGO2APSIM(data_soils = list_of_soils,
             area_threshold = 0.1,
             badge_name = 'Soils_3states_soilwat',
             path = "./APSIM_soils/",
             crops = tolower(c("Maize","Soybean")))



shinyApp(ui, server)
class(ui)
summary(ui)

length(ui)
str(ui)
ui[[1]]
ui[1][[1]]
   