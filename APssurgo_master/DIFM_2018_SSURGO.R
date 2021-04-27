library(soilDB)
library(sf)
library(DIFM)

# http://ncss-tech.github.io/AQP/

data_dir = "C:/Users/rodrigo7/Documents/DIFM/2018/DataBase"
farms = list.dirs(data_dir, recursive = FALSE, full.names = FALSE)


farm = farms[4]
for(farm in farms){
  print(farm)
  farm_dir = file.path(data_dir, farm)
  b_file = file.path(farm_dir, 'Boundary.gpkg')
  
  pol_sf = read_sf(b_file)
  
  ssurgo_pol = mapunit_geom_by_ll_bbox(st_bbox(pol_sf))
  ssurgo_sf = st_as_sf(ssurgo_pol)
  ssurgo_sf = coordTransform(ssurgo_sf)
  st_agr(ssurgo_sf) = 'constant'

  pol_sf = coordTransform(pol_sf)
  pol_sf = st_union(pol_sf)
  pol_sf = st_buffer(st_buffer(pol_sf, 1), -1)

  ssurgo_poli = st_intersection(ssurgo_sf, st_geometry(pol_sf))
  ssurgo_poli = st_transform(ssurgo_poli, st_crs(4326))
  
  mkey = unique(ssurgo_poli$mukey)
  in.statement = format_SQL_in_statement(mkey)
  q = paste("SELECT mukey, muname FROM mapunit WHERE mukey IN ", in.statement, sep="")
  res = SDA_query(q)
  
  ssurgo_poli = merge(ssurgo_poli['mukey'], res)
  save_file = file.path(farm_dir, 'SSURGO.gpkg')
  write_sf(ssurgo_poli, save_file)
  
}


