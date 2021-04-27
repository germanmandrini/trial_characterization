#COLLECT AND MERGE THE DATA
# library(lubridate)
library(tools)

apsim_merge_data <- function(directory_files, directory_output){
  # directory_output= paste0('./vr_value/Data/initial_conditions/cell_', id10_n)
  # directory_output= paste0('./vr_value/Data/yc_output/cell_', id10_n)
  flist = list.files(directory_files, full.names = TRUE, recursive = TRUE, pattern = '.out')
  results_collection_ls <- list()
  
  for(out_file_n in flist){
    #out_file_n <- flist[1]

    #Get the name of the simulation
    path_split <- strsplit(out_file_n, "/")[[1]]
    trial_n <- path_split[length(path_split)-1]
    year_n <- strsplit(trial_n, "_")[[1]][2]
    
    
    # results_collection_ls <- list()
    # for(out_file_n in out_files){
      # out_file_n <- out_files
      
      # name_sim <- gsub(".out$", "", basename(out_file_n))
      res <- try(fread(out_file_n, header = T), TRUE)
      
      res <- res[-1, ]
        
      if ("Date" %in% colnames(res)) {
        res$Date <- as.Date(res$Date, format = c("%d/%m/%Y"))
      }
      
      exclude <- c('Date', 'stage', 'stage_name', "paddock.soybean.stage","paddock.maize.stage", "FloweringDAS")
      res_col_names <- names(res)[!names(res) %in% exclude]
      
      res[,res_col_names,with = F]
      
      res[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names]
      
      names(res) <- gsub('(\\()([0-9]+)(\\))$', '_\\2', names(res))
      names(res) <- gsub('\\()', '', names(res))
      
      res <- cbind(data.table(trial = trial_n, year_trial = year_n),
                   res)
      
      sapply(res, class)
      results_collection_ls[[length(results_collection_ls)+1]] <- res
    }# end of out_file loop
    
    #SAVE THE OUTPUT
    
      if(!file.exists(directory_output)){ dir.create(directory_output, recursive = TRUE) }
      
      saveRDS(rbindlist(results_collection_ls, fill = TRUE), paste0(directory_output,'/output.rds'))
      
}
