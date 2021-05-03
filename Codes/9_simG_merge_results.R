#COLLECT AND MERGE THE DATA
library(data.table)
library(stringr)
library(tools)
# library(dplyr)

# setwd('/projects/aces/germanm2/')
# if(cpsc){setwd('C:/Users/germanm2/Box Sync/My_Documents')}#CPSC

apsim_merge_data <- function(out_file_n){
  # directory_output= paste0('./vr_value/Data/initial_conditions/cell_', id10_n)
  # directory_output= paste0('./vr_value/Data/yc_output/cell_', id10_n)
  # out_file_n = out_files_dt$path[1]
  
  # results_collection_ls <- list()
  res <- try(fread(out_file_n, header = T), TRUE)
  
  res <- res[-1, ]
  
  if ("Date" %in% colnames(res)) {
    res$Date <- as.Date(res$Date, format = c("%d/%m/%Y"))
  }
  
  # save_SP <- str_detect(out_file_n, pattern = '_0.out') #save all the SP for the one that has 0 N (just to pick one)
  
  # if(!save_SP){
  #   res <- res[year > 2009]
  # }
  
  exclude <- c('Date', 'stage', 'stage_name')
  res_col_names <- names(res)[!names(res) %in% exclude]
  
  suppressWarnings( res[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names])
  
  names(res) <- gsub('(\\()([0-9]+)(\\))$', '_\\2', names(res))
  names(res) <- gsub('\\()', '', names(res))
  
  name_sim <- basename(file_path_sans_ext(out_file_n))
  info <- strsplit(name_sim, split = '_')[[1]]
  res <- cbind(data.table(sim_name = name_sim,
                          id_10 = info[1],
                          mukey = info[2],
                          z = info[3], water = info[5]),    res)
  
  
}# end of out_file loop
#COLLECT AND MERGE THE DATA
# library(lubridate)


# id10_n = as.numeric(commandArgs(trailingOnly=TRUE)[1])
# if(cpsc) id10_n = 561


# directory_cell <- paste('./trial_characterization/apsim_temp/cell', id10_n, sep = '')
# if(cpsc){directory_cell <- paste('C:/apsim_temp/CPSC-P10E53323/trial_characterization/cell', id10_n, sep = '')}
# directory_cell <- paste('./cell', id10_n, sep = '')

out_files_dt <- data.table(path = list.files(directory, pattern = '.out', full.names = T, recursive = T)) #all out files in one folder
out_files_dt[,basename_f := file_path_sans_ext(basename(path))]

# out_files_dt[,mukey := sapply(strsplit(basename_f, split="_"), "[", 2) ]
# out_files_dt[,z := as.integer(sapply(strsplit(basename_f, split="_"), "[", 3)) ]
# out_files_dt[,mukey_z := paste(mukey, z, sep = '_')]

# out_files_dt2 <- data.table()
# mukey_seq <- unlist(unique(out_files_dt$mukey))
# 
# for(mukey_n in mukey_seq){
#   # mukey_n <- mukey_seq[1]
  # out_files_tmp <- out_files_dt[mukey == mukey_n]$path
  
  results_collection_ls <- lapply(out_files_dt$path, function(out_file_n) apsim_merge_data(out_file_n))
  
  #SAVE THE OUTPUT
  file_output_name <- paste('./trial_characterization_box/Data/','yc_output/trial', trial_n, '.rds', sep = '')
  # if(cpsc){file_output_name <- paste('S:/Bioinformatics Lab/germanm2/trial_characterization/',stab_or_yc, id10_n,"_",mukey_n, '.rds', sep = '')}
  
  if(!file.exists(dirname(file_output_name))){ dir.create(dirname(file_output_name), recursive = TRUE) }
  
  saveRDS(rbindlist(results_collection_ls, fill = TRUE), file_output_name)
  
