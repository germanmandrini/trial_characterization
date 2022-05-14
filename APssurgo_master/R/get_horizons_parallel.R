#===================================
# prepare clusters
#===================================


no_cores <- detectCores() * 6/8
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================
Int <- function (value){
  
  depth <- 1:length(value)
  
  if(all(is.na(value)) | all(is.infinite(value))) { 
    rep(NA, length(depth)) 
  }else if(length(unique(value[!is.na(value)])) > 1){
    
    approx(value,
           rule=2,
           xout=1:length(depth),
           method="linear")$y
    
    
  } else {
    
    rep(unique(value[!is.na(value)]),length(depth))
  }
  
}
value <- c(NA,NA)
Int(value)

data.table(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 15,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 7,NA,NA,NA,NA,NA),
           Int(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 15,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 7,NA,NA,NA,NA,NA)))

length(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 15,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 7,NA,NA,NA,NA,NA))

if(FALSE){
  mukey_n = "1716815"
  restrict_depth = FALSE
  soilLayer_breaks = c(5,10,15,20,40,60,80,100,150,200,250)
}

# mukey_n = soils_sf$mukey[1]
get_horizons <- function(mukey_n, soilLayer_breaks = c(5,10,15,20,40,60,80,100,150,200,250), restrict_depth = FALSE){
  max_soil_depth <- max(soilLayer_breaks)
  packages_need <- c('sf', 'soilDB', 'dplyr', 'data.table')
  lapply(packages_need, require, character.only = TRUE)
  
  #COMPONENT: each polygon in the map has a map unit. Each map unit (mukey) has possible major and minor components (soils)(cokey)
  q <- paste("SELECT\n  mukey, cokey, comppct_r, compname, drainagecl, slope_r, hydgrp, taxclname FROM component\n  WHERE mukey ='", mukey_n, "'", sep = "")
  
  # run the query
  component <- SDA_query(q) %>% data.table()
  if(nrow(component) < 1){return()}
  
  # comppct_r (Representative Percent Composition)
  component_major <- component[order(-comppct_r)] %>%
    .[1,c("compname", "comppct_r", "taxclname", "drainagecl", "mukey","cokey","slope_r","hydgrp")]
  
  #CHORIZON: soil profile of each of the components (cokey)
  q2 <- paste("SELECT\n  cokey, hzname, hzdept_r, hzdepb_r, hzthk_r, sandtotal_r, claytotal_r, om_r,
              dbthirdbar_r, ksat_r, wthirdbar_r, wfifteenbar_r,ph1to1h2o_r FROM chorizon\n  WHERE cokey ='", component_major[1, cokey], "'", sep = "")
  
  # run the query
  chorizon <- SDA_query(q2) %>% data.table() 
  if(nrow(chorizon) < 2){return()}
  chorizon <- chorizon[order(hzdept_r)]
  
  ## WATERTABLE: Water Table Depth - April - June Minimum in cm
  q3 <- paste("SELECT\n  mukey, wtdepaprjunmin FROM muaggatt\n  WHERE mukey ='", mukey_n, "'", sep = "")
  
  # run the query
  watertable <- SDA_query(q3) %>% data.table()
  
  # RESTRICTIONS: resdept_r = The distance from the soil surface to the upper boundary of the restrictive layer.R means RV (representative value)
  q4 <- paste("SELECT\n  cokey, resdepb_r FROM corestrictions\n  WHERE cokey ='", component_major[1, cokey], "'", sep = "")
  
  # run the query
  restrictions <- SDA_query(q4) %>% data.table()
  
  # if there are no restrictions, make it max_soil_depth
  if(nrow(restrictions) == 0){
    restrictions <- data.table(cokey = component_major[1, cokey], resdepb_r = max_soil_depth)
  }
  
  if(nrow(restrictions) > 1){ restrictions <- restrictions[1,]} #avoid an error if it gets more than restriction
  
  
  if((is.na(restrictions$resdepb_r)) | (restrictions$resdepb_r > max_soil_depth)){
    restrictions <- data.table(cokey = component_major[1, cokey], resdepb_r = max_soil_depth)
  }
  
  old_cols = c("mukey","slope_r", "hydgrp","hzname",
               "hzdept_r","hzdepb_r","hzthk_r", # _r means representative value
               "sandtotal_r","claytotal_r",
               "dbthirdbar_r",
               "om_r","ksat_r",
               "wfifteenbar_r","wthirdbar_r","ph1to1h2o_r","wtdepaprjunmin", 'resdepb_r')
  
  new_cols <- c("mukey", "slope","hydrogroup","hzname",
                "top","bottom","thick",
                "sand","clay",
                "wetbd",
                "om","ksat",
                "ll","dul","ph","watertable", "restriction")
  
  #Each row is a layer of the soil. Only major soils included
  h <- component_major %>%
    merge(chorizon, by = c("cokey")) %>%
    merge(watertable, all.x = TRUE, by = c("mukey")) %>%
    merge(restrictions, all.x = TRUE, by = c("cokey")) %>%
    .[order(compname, hzdept_r)] %>%
    .[,old_cols, with = FALSE] %>%
    setnames(old_cols, new_cols)
  
  # Bin slope groups
  hydrogroup <- readRDS("./Trial_crct_DIFM/Data/APssurgo_master/R/hydrogroup.rds")
  h <- h[,slope_code := .bincode(slope, breaks=c(-0.01,2,5,10,100))] %>% #had to put -0.01 to avoid NA when slope is 0
    merge(hydrogroup, by = c("hydrogroup", "slope_code"), all.x = TRUE)
  
  h2 <- h %>%
    .[,thick := ifelse(is.na(thick),bottom - top,thick)] %>%
    .[,center := trunc(top + thick/2)]
  
  #-------------------------------------------------------------------------------------------------------------------------
  # If one of the layers is na (expect ph) jump
  cols <- c("sand","wetbd","ksat","clay", "om","ll","dul")
  
  if(any(sapply(h2[, cols, with = F], function(x) all(is.na(x))))){return()}
  
  
  #-------------------------------------------------------------------------------------------------------------------------
  #Expand each layer
  expand_vector = rep(1:nrow(h2), times=h2$thick) #repeat each row for each cm it has in the soil
  table(expand_vector)
  length(expand_vector)
  
  # Correct the expand_vector. Has to have a lenght equal to max_soil_depth. Will correct for restrictions later
  if(length(expand_vector) > max_soil_depth){
    expand_vector <- expand_vector[1:max_soil_depth]
  }
  
  if(restrict_depth == FALSE & (length(expand_vector) < max_soil_depth)){
    expand_vector <- c(expand_vector,
                       rep(max(expand_vector), max_soil_depth - length(expand_vector)))
  }
  
  h3 <- h2[expand_vector[1:max_soil_depth]]
  h3[, center_n := (seq_len(.N)), by=mukey]
  h3[center != center_n, c("sand","wetbd","ksat","clay", "om","ll","dul","ph") := NA] #leave the original value at the center and interpolate the others
  
  h4 <- h3[,-c('hzname', 'center')] %>% setnames('center_n', 'center')
  
  cols <- c("sand","wetbd","ksat","clay", "om","ll","dul","ph")
  
  h4[, (cols) := lapply(.SD, function(x) Int(x)), .SDcols = cols] #interpolate values from the center of the layer to the other cm
  
  # Update the maxdepth
  # h4[,maxdepth := max(center)]
  h4$mukey = as.character(h4$mukey)
  #------------------------------------------------------------------------------------------------------------------------
  # Bin by layer
  h4[,layer := .bincode(center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))]
  
  horizon_grouped <- h4[,-c('hydrogroup')] %>% .[, lapply(.SD,mean), by=.(mukey, layer)]
  
  # average_soil_dt <- horizon_grouped[,-'mukey'] %>% .[, lapply(.SD, weighted.mean, w = area_pct), by=.(layer)] %>% .[,mukey := 'Average']
  
  # soils_dt <- rbind(horizon_grouped, average_soil_dt)
  
  correction_dt <- data.table(layer = sort(unique(horizon_grouped$layer)),
                              top = c(0, soilLayer_breaks[-length(soilLayer_breaks)]),
                              bottom = soilLayer_breaks)
  
  horizon_grouped <- merge(horizon_grouped[,-c('top', 'bottom', 'thick', 'center')],
                           correction_dt, by = 'layer') %>% .[order(mukey,layer)] %>% setcolorder(c('mukey','layer'))
  
  if(restrict_depth){
    horizon_grouped <- horizon_grouped[top < restriction] #remove a layer whose top is below the restriction limit
    horizon_grouped[bottom > restriction, bottom := round(restriction,0)]
  }
  
  horizon_grouped[,thick := (bottom-top)*10]
  horizon_grouped[,center := top+thick/10/2]
  
  setcolorder(horizon_grouped, c('mukey', 'top', 'bottom', 'thick', 'center', 'restriction', 'watertable'))
  
  return(horizon_grouped)
}

soils_sf <- readRDS('./trial_characterization_box/Data/rds_files/soils_sf.rds')

keep <- c('keep','Int', 'get_horizons')

clusterExport(cl, varlist = keep, envir=environment())

results_list <- parLapply(cl, unique(soils_sf$mukey), function(x) get_horizons(x))

results_list_clean <- results_list[vapply(results_list, Negate(is.null), NA)]

horizons_dt <- data.table::rbindlist(results_list_clean)

info_dt <- data.table(soils_sf) %>% .[,.(id_loc, mukey, X, Y)]
info_dt[,mukey := as.character(mukey)]
horizons_dt <- merge(horizons_dt, info_dt, by = c('mukey')) %>% setcolorder(c('id_loc', 'mukey', 'X', 'Y'))

saveRDS(horizons_dt, "./trial_characterization_box/Data/rds_files/horizons_dt.rds")

# results_list <- list()
# for(mukey_n in sort(unique(grid10_soils_v2_sf$mukey))[2742:length(unique(grid10_soils_v2_sf$mukey))]){
#   print(mukey_n)
#   results_list[[mukey_n]] <- get_horizons(mukey_n)
# }
# which(sort(unique(grid10_soils_v2_sf$mukey)) == mukey_n)
stopCluster(cl)
