#===================================
# prepare clusters
#===================================


no_cores <- detectCores() - 1
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================

# mukey_n = fields_sf$mukey[10]
get_components <- function(mukey_n){
  packages_need <- c('sf', 'soilDB', 'dplyr', 'data.table')
  lapply(packages_need, require, character.only = TRUE)
  
  #COMPONENT: each polygon in the map has a map unit. Each map unit (mukey) has possible major and minor components (soils)(cokey)
  q <- paste("SELECT\n  mukey, cokey, comppct_r, compname, drainagecl, slope_r, hydgrp, taxclname FROM component\n  WHERE mukey ='", mukey_n, "'", sep = "")

  # run the query
  component <- SDA_query(q) %>% data.table()
  
  # comppct_r (Representative Percent Composition)
  component_major <- component[order(-comppct_r)][1,]
  ## WATERTABLE: Water Table Depth - April - June Minimum in cm
  q3 <- paste("SELECT\n  mukey, wtdepaprjunmin FROM muaggatt\n  WHERE mukey ='", mukey_n, "'", sep = "")

  # run the query
  watertable <- SDA_query(q3) %>% data.table()

  component2 <- merge(component, watertable, by = 'mukey')
  
  old_cols <- c("mukey","slope_r", "hydgrp", 'wtdepaprjunmin')

  new_cols <- c("mukey", "slope","hydrogroup", 'watertable')

  data.table::setnames(component2, old_cols, new_cols)
  
  return(component2)
}


keep <- c('keep', 'get_components')

clusterExport(cl, varlist = keep, envir=environment())

results_list <- parLapply(cl, unique(fields_sf$mukey), function(x) get_components(x))

# fields <- list()
# for(cell_n in ids_10_seq[0:20]){
#   print(cell_n)
#   fields[[cell_n]] <- process_cells(cell_n)
# }

x <- data.table::rbindlist(results_list)

stopCluster(cl)

