#x <- readRDS("x.rds")

Int <- function (value){
  
  depth <- 1:length(value)
  
  if(all(is.na(value)) | all(is.infinite(value))) { rep(NA, length(depth)) }
  
  if(length(unique(depth[!is.na(depth)])) > 1){
    
    approx(depth, value, 
           rule=2,
           xout=1:length(depth)-1,
           method="linear")$y
  }
  
  else {
    
    rep(unique(depth[!is.na(depth)]),length(depth)) 
  }
}



# remove.factors <- function(df){
#   for (varnum in 1:length(df)) {
#     if ("factor" %in% class(df[, varnum])) {
#       df[varnum] = as.character(df[, varnum])
#     }
#   }
#   return(df)
# }

#df <- x$tabular$chorizon
normalize_dt <- function(df, mukey_musym_link){
  dt <- data.table(df)
  dt <- suppressWarnings(dt[,-'geometry'])
  #make names lower case
  names(dt) <- tolower(names(dt))
  #select factor and also mukey or musym to make it character
  cols <- names(dt)[sapply(dt,is.factor)]
  cols <- unique(c(cols, names(dt)[names(dt) %in% c('musym', 'mukey', 'cokey')]))
  if(length(cols) > 0){
  dt[,(cols) := lapply(.SD, as.character),.SDcols=cols]
  }
  
  #included <- names(dt)[names(dt) %in% c('mukey', 'musym')]
  #dt2 <- merge(dt, mukey_musym_link, by = included, all.x = TRUE)
  
  return(dt)
}

# add_mukey_musey_link <- function(dt, mukey_musym_link){
#   included <- names(dt)[names(dt) %in% c('mukey', 'musym')]
#   dt2 <- merge(dt, mukey_musym_link, by = included, all.x = TRUE)
# }


test = FALSE
if(test){
  soilLayer_breaks = c(20,40,60,150,200)
  extend_to_limit = TRUE
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

moldSSURGO_gm <- function(x, soilLayer_breaks = c(20,40,60,150,200), extend_to_limit = FALSE){
  
  #stop(class(a) != "SpatialPolygons", "Needs a 'SpatialPolygons' object")
  
  app_map_union <- suppressWarnings(st_sfc(st_combine(app_map)) %>% st_buffer(0))
  
  spatial_sf <- suppressWarnings(st_as_sf(x$spatial) %>% st_intersection(app_map_union))
  
  tm_shape(a) + tm_polygons()+
    tm_shape(spatial_sf) + tm_polygons(c('MUKEY')) +
    tm_layout(main.title = 'Map units and extension')
  
  spatial_sf$area_ha <- round(as.numeric(st_area(spatial_sf))/10000,2)
  
  mapunit_major <- data.table::data.table(spatial_sf %>% dplyr::select(MUKEY, area_ha)) %>% 
    .[,.(area_ha = sum(area_ha)), by = MUKEY] %>% .[order(-area_ha)] %>% .[,area_pct := area_ha/sum(area_ha)]
  
  # Extract useful data from
  
  #COMPONENT: each polygon in the map has a map unit. Each map unit (mukey) has possible major and minor components (soils)(cokey)
  component <- x$tabular$component %>% normalize_dt()#each row in the table is a soil in the mapunit. Only one is the major component
  
  # we select it and also select the important columns that describes it
  # comppct_r (Representative Percent Composition) 
  component_major <- component[mukey == as.character(mapunit_major[1,MUKEY])] %>% 
    .[order(-comppct.r)] %>% 
    .[1,c("compname", "comppct.r", "taxclname", "drainagecl", "mukey","cokey","slope.r","hydgrp")]
  
  # length(unique(component$mukey)) #mukey is the map unit key. Each polygon
  # length(unique(component$cokey)) #the mukey has 3 major components and some minor. They have a cokey, component unit key
  # count_of_cokey <- unique(component[,.(mukey, cokey)]) %>% .[,.(cokey_count = .N), by = mukey]
  # count_of_majcokey <- unique(component[,.(mukey, cokey,majcompflag)]) %>% .[majcompflag == 'Yes',.(maj_cokey = .N), by = mukey]
  # merge(count_of_cokey, count_of_majcokey) #beautiful!
  
  #CHORIZON: soil profile of each of the components (cokey)
  chorizon <- x$tabular$chorizon %>% normalize_dt()
  chorizon_major <- chorizon[cokey == component_major$cokey,   c("cokey","hzname","hzdept.r","hzdepb.r","hzthk.r","sandtotal.r","claytotal.r","om.r",
                                                                 "dbthirdbar.r","dbovendry.r","ksat.r","wthirdbar.r","wfifteenbar.r","ph1to1h2o.r")]
  
  # major_components <- unique(component[,.(mukey, cokey,majcompflag)]) %>% .[majcompflag == 'Yes']
  # chorizon_presence <- major_components[,chorizon_presence := ifelse(cokey %in% unique(chorizon$cokey), 1,0)]
  
  # mapunit <-x$tabular$mapunit %>% normalize_dt()
  # mapunit_major <- mapunit[mukey == as.character(mapunit_major[1,MUKEY])]
  
  watertable <- x$tabular$muaggatt[ , c("mukey","wtdepaprjunmin")] %>% #Water Table Depth - April - June Minimum in cm
    normalize_dt()
  
  # resdept_r = The distance from the soil surface to the upper boundary of the restrictive layer.R means RV (representative value)
  restrictions <- x$tabular$corestrictions %>% normalize_dt()
  restrictions_major <- restrictions[, .(cokey, resdepb.r)] %>% .[cokey == component_major$cokey]
  
  # majcompflag = Indicates whether or not a component is a major component in the mapunit.
  # we select it and also select the important columns that describes it
  
  # majcomp <- component[majcompflag == "Yes", c("compname", "comppct.r", "taxclname", "drainagecl",
  #                                              "mukey","cokey","slope.r","hydgrp")]# comppct_r : representative percent composition
  # length(unique(component$mukey))
  # length(unique(component$compname))
  # length(unique(majcomp$mukey))
  # length(unique(majcomp$compname))
  
  # component[,.(count = .N),by=mukey] #some mukey have more than one cokey
  # majcomp[,.(count = .N),by=mukey]
  
  # #we merge with the mukey and the musey info
  #   # x$spatial@data %>%
  #   #   mutate(MUKEY = as.numeric(as.character(MUKEY)),
  #   #          MUSYM = as.character(MUSYM)) %>%
  #   #   left_join(majcomp %>%
  #   #               mutate(mukey = as.numeric(as.character(mukey))), by = c("MUKEY" = "mukey")) -> majcomp2 
  # 
  
  # Calculate % of area
  # spatial_data.sf <- st_as_sf(x$spatial) %>% dplyr::mutate(mukey = as.character(MUKEY)) %>% dplyr::select(-MUKEY)
  # spatial_data.sf$area <- as.numeric(st_area(spatial_data.sf))
  # spatial_data.dt <- spatial_data.sf %>% normalize_dt() 
  # 
  # tm_shape(spatial_data.sf) + tm_polygons(as.character('mukey'))
  # 
  # sort(unique(spatial_data.dt$mukey))
  # sort(unique(majcomp$mukey))
  # 
  # area <- spatial_data.dt %>% merge(majcomp, all.x = TRUE) %>% .[,.(area_2 = sum(area)), by = c('mukey','musym','compname')] %>%
  #   .[,area := round(area_2/sum(area_2),10)] %>% .[,-('area_2')]
  
  # area %>% 
  #   left_join(majcomp, by = c("MUSYM" = "musym")) %>%
  #   group_by(MUSYM,compname) %>% 
  #   summarise(area=sum(area)) %>%
  #   group_by() %>%
  #   mutate(area = area/sum(area)) %>%
  #   as.data.frame() %>%
  #   `names<-`( c("musym","compname","area"))-> area
  
  # Merge into dataset
  old_cols = c("compname","slope.r", "hydgrp","hzname",
                  "hzdept.r","hzdepb.r","hzthk.r", # .r means representative value
                  "sandtotal.r","claytotal.r",
                  "dbthirdbar.r","dbovendry.r", 
                  "om.r","ksat.r",
                  "wfifteenbar.r","wthirdbar.r","ph1to1h2o.r","wtdepaprjunmin", 'resdepb.r')
  
  new_cols <- c("name","slope","hydrogroup","hzname",
                "top","bottom","thick",
                "sand","clay",
                "wetbd","drybd", 
                "om","ksat",
                "ll","dul","ph","watertable", "restriction")
  
  #Each row is a layer of the soil. Only major soils included
  h <- component_major %>%
    merge(chorizon_major, by = c("cokey"))%>%
    merge(watertable, all.x = TRUE, by = c("mukey")) %>%
    merge(restrictions_major, all.x = TRUE, by = c("cokey")) %>%
    .[order(compname, hzdept.r)] %>%
    .[,old_cols, with = FALSE] %>%
    setnames(old_cols, new_cols) #%>%
    #.[,watertable_g := ifelse(is.na(watertable), NA, watertable + 100)]
  
  # Bin slope groups
  hydrogroup <- readRDS("R/hydrogroup.rds")
  h <- h[,slope_code := .bincode(slope, breaks=c(0,2,5,10,100))] %>%
    merge(hydrogroup, by = c("hydrogroup", "slope_code"), all.x = TRUE)
  
  h2 <- h %>% 
    .[,thick := ifelse(is.na(thick),bottom - top,thick)] %>%
    .[,center := trunc(top + thick/2)] %>%
    .[,maxdepth := max(bottom), by = name]
  
  # h %>%
  # left_join(hydrogroup) %>%
  # mutate(thick = ifelse(is.na(thick),bottom - top,thick),
  #        center = trunc(top + thick/2)) %>%
  # group_by(name) %>%
  # mutate(maxdepth  = max(bottom)) -> h 
  
  if(extend_to_limit == TRUE){
  # Add extra layer to the bottom if not deep enough. It has same properties than the last layer described by Ssurgo
  
    h3 <- h2[bottom == maxdepth,] #filter the last layer
    h3[,lower_lim := ifelse(is.na(restriction),max(soilLayer_breaks),
                            min(max(soilLayer_breaks), restriction))]
    
    if(any(h3$maxdepth < h3$lower_lim)) {
      h4 <- h3[maxdepth < lower_lim,]
      h4$top <- h4$bottom
      h4$bottom <- h4$lower_lim
      h4$thick <- h4$bottom - h4$top
      h4[,center := trunc(top + thick/2)]
      
      h2 <- rbind(h2,h4[,-'lower_lim'])
      h2 <- h2[order(name, bottom)]
  }
  }#end of extend_limitorbreak
  max_depth_correction <- h2[,.(maxdepth = max(bottom)), by = name]
  h2 <- merge(h2[,-'maxdepth'], max_depth_correction, by = 'name')
  
  #h2 <- h2[name == 'Crider_PcC3']
  expand_vector = rep(1:nrow(h2), times=h2$thick) #repeat each row for each cm it has in the soil
  table(expand_vector)
  length(expand_vector)
  h3 <- h2[expand_vector]
  h3[, center_n := (seq_len(.N)), by=name]
  h3[center != center_n, c("sand","drybd","wetbd","ksat","clay", "om","ll","dul","ph") := NA] #leave the original value at the center and interpolate the others
  
  h4 <- h3[,-c('hzname', 'watertable', 'center')] %>% setnames('center_n', 'center')
  
  cols <- c("sand","drybd","wetbd","ksat","clay", "om","ll","dul","ph")
  h4[, (cols) := lapply(.SD, function(x) Int(x)), .SDcols = cols] #interpolate values from the center of the layer to the other cm
  h4[,layer := .bincode(center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))]
  setcolorder(h4, 'name')
  return(h4)
}
 

# 
# pre_original %>% data.table()  %>% .[name == 'Crider_PcC3',] %>% .[!is.na(clay)]
# h3  %>% .[name == 'Crider_PcC3',] %>% .[!is.na(clay)]
# 
# pre_original_test <- pre_original %>% data.table()  %>% .[name == 'Crider_PcC3',] 
# new_test <- h3  %>% .[name == 'Crider_PcC3',]
# 
# Int(pre_original_test$clay)
# Int(new_test$clay)
# 
# original <- h %>% data.table()  %>% .[name == 'Crider_PcC3',] %>% .[center <= 135,] %>% .[order(center)]
# test <- h4[name == 'Crider_PcC3'] %>% .[order(center)] %>% .[center <= 135,]
# 
# compare_cols <- intersect(names(original), names(test))  
# setdiff(c(names(original), names(test)), compare_cols) 
# 
# result.dt <- data.table()
# for(col in compare_cols){
#   #col = 'sand'
#   equal_val = all(original[[col]] == test[[col]])
#   result.dt <- rbind(result.dt, data.table(var = col, same = equal_val))
#   if(!equal_val){
#     print(paste('----------', col, '----------'))
#     print(original[[col]])
#     print(test[[col]])
#     
#   }
# }
# original 
# nrow(pre_original)
# original <- data.table(original)  %>% .[name == 'Crider_PcC3',]
# setdiff(1:250, original$center)
# 
# test <- test %>% .[name == 'Crider_PcC3',]<- moldSSURGO(x,soil_layers)
# test <- moldSSURGO_gm(x,soil_layers, extend_to_last_break = TRUE)
# 
# 
