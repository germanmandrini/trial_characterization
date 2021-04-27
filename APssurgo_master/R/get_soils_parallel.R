#===================================
# prepare clusters
#===================================


# no_cores <- detectCores() - 4
# cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================

# loc_n=3

get_soils <- function(loc_n){
  packages_need <- c('sf', 'soilDB', 'dplyr', 'data.table')
  lapply(packages_need, require, character.only = TRUE)
  
  one_loc_sf <- locs_sf[loc_n,]
  
  one_loc_box <- st_bbox(one_loc_sf)
  one_loc_box[[3]] <- one_loc_box[[1]]+ 0.00001
  one_loc_box[[4]] <- one_loc_box[[2]]+ 0.00001
  
  possibleError <- tryCatch({
    ssurgo_pol <- mapunit_geom_by_ll_bbox(one_loc_box)
    
    ssurgo_sf <- st_as_sf(ssurgo_pol)
    st_crs(ssurgo_sf) <- 4326
    # ssurgo_sf <- st_transform(ssurgo_sf, 4326)
    ssurgo_sf <- dplyr::select(ssurgo_sf, musym, mukey)
  
    # ssurgo_sf_utm <- st_utm(ssurgo_sf)
    # one_loc_sf_utm  <- st_utm(one_loc_sf)
    
    field_soils_tmp <- st_intersection(ssurgo_sf,one_loc_sf )
    
    tm_shape(ssurgo_sf) + tm_polygons('mukey') +
      tm_shape(field_soils_tmp) + tm_dots(size = 2)
    
    # tm_shape(field_soils_tmp) + tm_polygons('mukey') +
    #   tm_layout(main.title = 'Map units')# + tm_text('mukey')
  
    field_soils_tmp$area_ha <- round(as.numeric(st_area(field_soils_tmp))/10000,6)
  }, error = function(e) e)
  
  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){
    return(field_soils_tmp)
  } else {
    return()
  }
}

# source('./vr_value/Codes/functions_vr.R')

# keep <- c('keep', 'one_tile_sf','get_soils', 'st_utm')

# clusterExport(cl, varlist = keep, envir=environment())

# results_list <- parLapply(cl, 1:nrow(one_tile_sf), function(x) get_soils(x))

# get_soils(1)
results_list <- list()
for(loc_n in 1:nrow(locs_sf)){
  print(loc_n)
  results_list[[loc_n]] <- get_soils(loc_n)
}

results_list_clean <- results_list[vapply(results_list, Negate(is.null), NA)]

soils_sf <- do.call(what = base::rbind, args = results_list_clean)
rownames(soils_sf) <- 1:nrow(soils_sf)

saveRDS(soils_sf, './trial_characterization_box/Data/rds_files/soils_sf.rds') 
# stopCluster(cl)
  