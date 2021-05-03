rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
# setwd('~')#Server
# codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# ---------------------------------------------------------------------------------------------
# Load the input file, with coordinates and planting dates

trials_dt <- data.table::fread('./trial_characterization_box/Data/input.csv') 
trials_dt[,Crop := tolower(Crop)]
trials_sf = st_as_sf(trials_dt, coords = c("Longitude", "Latitude"), 
                     crs = 4326, agr = "constant")

us_states4326.sf <- st_transform(us_states, 4326) %>% dplyr::select(NAME, REGION)


(plot1 <- tm_shape(us_states4326.sf) + tm_polygons()   +
  tm_shape(trials_sf) + tm_dots(size = 0.2))

tmap_save(plot1, 
          filename = "./trial_characterization_box/Data/output/map.pdf", height = 8, width = 6)  


# Intersect with US map, in case there are some trials in other countries. We may not have SSURGO and DAYMET data in those
trials_sf <- st_intersection(trials_sf, us_states4326.sf)


# Add coordinates and id_trial (unique identifier for trial x year)
trials_sf <- cbind(trials_sf, st_coordinates(trials_sf)) %>% 
        mutate(id_trial = row_number())

# Find unique fields and called them location. A trial is an year x loc combination

locs_sf <- st_difference(trials_sf) %>% mutate(id_loc = row_number()) %>% dplyr::select(id_loc, X,Y)
  

# Add the id_loc to the trials
trials_sf <- st_join(trials_sf, dplyr::select(locs_sf, id_loc))

trials_sf <- trials_sf %>% dplyr::mutate(planting_date = as.Date(Planting, format = "%m/%d/%Y"),
                                         day = format(planting_date,"%d"),
                                         month = tolower(format(planting_date,"%b")),
                                         year = format(planting_date,"%Y")) 


saveRDS(trials_sf, './trial_characterization_box/Data/rds_files/trials_sf.rds')
saveRDS(locs_sf, './trial_characterization_box/Data/rds_files/locs_sf.rds')
input_clean_dt <- data.table(trials_sf) %>% .[,-c('geometry', 'id_loc')] %>%
  setcolorder(c('id_trial'))
data.table::fwrite(input_clean_dt, './trial_characterization_box/Data/output/input_clean.csv')
