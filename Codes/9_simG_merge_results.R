#COLLECT AND MERGE THE DATA
library(data.table)
library(stringr)
library(tools)
# library(dplyr)

apsim_merge_data <- function(out_file_n){
  # out_file_n = out_files_dt$path[111]
  
  #Information from file name
  name_sim <- basename(file_path_sans_ext(out_file_n))
  trial_n <- as.integer(strsplit(name_sim, split = '_')[[1]][[2]])
  crop_n <- strsplit(name_sim, split = '_')[[1]][[3]]
  
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
  
  exclude <- c('Date', 'stage', 'stage_name', 'stagename', 'paddock.soybean.stagename','paddock.maize.stagename')
  res_col_names <- names(res)[!names(res) %in% exclude]
  
  suppressWarnings( res[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names])
  
  # names(res) <- gsub('(\\()([0-9]+)(\\))$', '_\\2', names(res))
  # names(res) <- gsub('\\()', '', names(res))
  
  res[,id_trial := trial_n]
  res[,crop := crop_n]
  setcolorder(res, c('id_trial', 'crop'))
  
  #remove other crop
  remove_this_crop <- ifelse(crop_n == 'soybean', 'maize', 'soybean')
  
  remove_this_columns <- names(res)[stringr::str_detect(string = names(res), pattern = remove_this_crop)]
  res[ , (remove_this_columns) := NULL]
  
  #remove word paddock from names
  names(res) <- gsub(x = names(res), pattern = paste0('paddock.', crop_n, '.'), replacement = '')
  
  
  
  return(res)
  
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
  
daily_dt <- rbindlist(results_collection_ls, fill = TRUE)
daily_dt <- daily_dt[order(id_trial)]

#SAVE THE OUTPUT

saveRDS(daily_dt, './trial_characterization_box/Data/rds_files/apsim_output_daily.rds')
data.table::fwrite(daily_dt, './trial_characterization_box/Data/output/apsim_output_daily.csv')
