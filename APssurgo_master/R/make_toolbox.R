# SSURGO2APSIM Function

source("./Trial_crct_DIFM/Data/APssurgo_master/R/compile_toolbox_v4.R")

test = FALSE
if(test){
  data_soils = all_soils
  badge_name = 'Soils_DIFM_bysoil'
  path = "./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/"
  crops = tolower(c("Maize","Soybean"))
}


make_toolbox <- function(data_soils,
                         badge_name,
                         path = "",
                         crops) {

  #message(paste0("Creating APSIM toolbox with soils that occupy an area greather than ",area_threshold*100,"% of the ",site_name)," site.")

  #if(!(area_threshold >= 0 & area_threshold <= 1)) stop("'area_threshold' must be between 0 and 1")

  #data$soils <- lapply(data$soils, function(x) {if(x[[1]] > area_threshold) return(x)})
  #data$soils <- data$soils[!sapply(data$soils, is.null)]

  out <- compile_toolbox(data_soils, badge_name, crops)

  writeLines(saveXML(out), paste0(path,badge_name, ".soils"))
}

# base_doc <- xmlParse('C:/Users/germa/Box Sync/APSIM Workshop 2018/day 1/day 1/Soils/SoilsISU2015.soils')

