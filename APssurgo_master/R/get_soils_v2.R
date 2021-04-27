#x <- readRDS("x.rds")

Int <- function (value){

  depth <- 1:length(value)

  if(all(is.na(value)) | all(is.infinite(value))) { rep(NA, length(depth)) }

  if(length(unique(depth[!is.na(depth)])) > 1){

        approx(value,
           rule=2,
           xout=1:length(depth),
           method="linear")$y


  }

  else {

    rep(unique(depth[!is.na(depth)]),length(depth))
  }
}

data.table(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 15,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 7,NA,NA,NA,NA,NA),
           Int(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 15,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 7,NA,NA,NA,NA,NA)))

length(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 15,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 7,NA,NA,NA,NA,NA))



if(FALSE){
  max_soil_depth = 250
  restrict_depth = FALSE
  app_map = trial_cleaned_map
}

#' Title
#'
#' @param x
#' @param soilLayer_breaks
#' @param extend_to_last_break
#'
#' @return
#' @export
#'
#' @examples

get_soils <- function(app_map, max_soil_depth = 250, restrict_depth = FALSE){

  ssurgo_pol = mapunit_geom_by_ll_bbox(st_bbox(app_map))
  ssurgo_sf = st_as_sf(ssurgo_pol)
  st_crs(ssurgo_sf)<- 4326
  ssurgo_sf <- dplyr::select(ssurgo_sf, musym, mukey)

  # ssurgo_sf <- st_intersection(dplyr::select(ssurgo_sf, musym, mukey), app_map)

  del_col <- which(names(trial_cleaned_map) %in% c('musym', 'mukey'))

  if(length(del_col) > 0){
    trial_cleaned_map <- trial_cleaned_map[,-del_col]
  }


  trial_cleaned_map <- st_join(trial_cleaned_map, ssurgo_sf, left = T, largest = T)

  #
  
  # app_map_union <- suppressWarnings(st_sfc(st_combine(app_map)) %>% st_buffer(0))
  # ssurgo_sf <- suppressWarnings(ssurgo_sf %>% st_intersection(app_map_union)) %>% st_buffer(0)
  # 
  # 
  # tm_shape(app_map_union) + tm_polygons()

  # 
  
  tm_shape(ssurgo_sf) + tm_polygons('mukey') +
    tm_layout(main.title = 'Map units')# + tm_text('mukey')

  tm_shape(trial_cleaned_map) + tm_polygons('mukey')+
    tm_layout(main.title = 'c) Soils (mukey)')# + tm_text('mukey')

  trial_cleaned_map$area_ha <- round(as.numeric(st_area(trial_cleaned_map))/10000,6)

  write_sf(trial_cleaned_map, file_n)

  # saveRDS(ssurgo_sf, paste('./Trial_crct_DIFM/Data/musym_maps/', farm, '_',field, '_musym_sf', sep = ''))

  mapunit_major <- data.table::data.table(trial_cleaned_map %>% dplyr::select(mukey, area_ha)) %>%
    .[,.(area_ha = sum(area_ha)), by = mukey] %>% .[order(-area_ha)] %>%
    .[,area_pct := round(area_ha/sum(area_ha),3)] %>% .[,area_cum := cumsum(area_pct)]

  # mukeys_70 <- mapunit_major[1:(mapunit_major[area_cum > 0.7, which = T][1]), mukey]
  mukeys_100 <- unique(mapunit_major$mukey)
  soils_profiles <- data.table()

  for(mukey_n in mukeys_100){
    # mukey_n <- mukeys_100[1]

    #COMPONENT: each polygon in the map has a map unit. Each map unit (mukey) has possible major and minor components (soils)(cokey)
    q <- paste("SELECT\n  mukey, cokey, comppct_r, compname, drainagecl, slope_r, hydgrp, taxclname FROM component\n  WHERE mukey ='", mukey_n, "'", sep = "")

    # run the query
    component <- SDA_query(q) %>% data.table()

    # comppct_r (Representative Percent Composition)
    component_major <- component[order(-comppct_r)] %>%
      .[1,c("compname", "comppct_r", "taxclname", "drainagecl", "mukey","cokey","slope_r","hydgrp")]

    #CHORIZON: soil profile of each of the components (cokey)
    q2 <- paste("SELECT\n  cokey, hzname, hzdept_r, hzdepb_r, hzthk_r, sandtotal_r, claytotal_r, om_r,
                dbthirdbar_r, dbovendry_r, ksat_r, wthirdbar_r, wfifteenbar_r,ph1to1h2o_r FROM chorizon\n  WHERE cokey ='", component_major[1, cokey], "'", sep = "")

    # run the query
    chorizon <- SDA_query(q2) %>% data.table() %>% .[order(hzdept_r)]

    ## WATERTABLE: Water Table Depth - April - June Minimum in cm
    q3 <- paste("SELECT\n  mukey, wtdepaprjunmin FROM muaggatt\n  WHERE mukey ='", mapunit_major[1, mukey], "'", sep = "")

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

    if((is.na(restrictions$resdepb_r)) | (restrictions$resdepb_r > max_soil_depth)){
      restrictions <- data.table(cokey = component_major[1, cokey], resdepb_r = max_soil_depth)
    }


    old_cols = c("mukey","slope_r", "hydgrp","hzname",
                 "hzdept_r","hzdepb_r","hzthk_r", # _r means representative value
                 "sandtotal_r","claytotal_r",
                 "dbthirdbar_r","dbovendry_r",
                 "om_r","ksat_r",
                 "wfifteenbar_r","wthirdbar_r","ph1to1h2o_r","wtdepaprjunmin", 'resdepb_r')

    new_cols <- c("mukey", "slope","hydrogroup","hzname",
                  "top","bottom","thick",
                  "sand","clay",
                  "wetbd","drybd",
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
    h3[center != center_n, c("sand","drybd","wetbd","ksat","clay", "om","ll","dul","ph") := NA] #leave the original value at the center and interpolate the others

    h4 <- h3[,-c('hzname', 'watertable', 'center')] %>% setnames('center_n', 'center')

    cols <- c("sand","drybd","wetbd","ksat","clay", "om","ll","dul","ph")

    h4[, (cols) := lapply(.SD, function(x) Int(x)), .SDcols = cols] #interpolate values from the center of the layer to the other cm

    # Update the maxdepth
    # h4[,maxdepth := max(center)]
    h4$mukey = as.character(h4$mukey)

    h4 <- merge(h4, mapunit_major[, .(mukey, area_pct)], by = 'mukey')
    setcolorder(h4, c('mukey', 'top', 'bottom', 'thick', 'center', 'restriction'))
    soils_profiles <- rbind(soils_profiles, h4)

  }

  return(soils_profiles)
}
