#******************************************************************
# A function that reads the downloaded data
#   - currently for monthly time series data files
#******************************************************************
# Inputs
#******************************************************************
# ts_path - the relative path to the folder containing run output
# datatype - type of data being read, e.g. tave, pave, pmax, ...
# subfolder_obs - the subdirectory with the obs data, e.g. 1_8obs
# subfolder_proj - the subdirectory with the projection data, e.g. bscd5
# observed_data_id - observed data prefix, e.g. 
#    Prcp - precip in mm from gridded data source
#    Tavg - average temperature in deg C from projections
#    tasmin - average temperature in deg C from projections
#    tasmax - average temperature in deg C from projections
# projection_data_id - observed data prefix, e.g.
#    pr - precip in mm from projections
#    tas - average temperature in deg C from projections
#    tasmin - average temperature in deg C from projections
#    tasmax - average temperature in deg C from projections
#******************************************************************
# Output
#******************************************************************
# A dataframe in "long" format, 
#   with columns: year, month, run, val, type
#******************************************************************
#
read_br_spatialave_func <- function(ts_path, datatype, 
                                    subfolder_obs, subfolder_proj, 
                                    observed_data_id, projection_data_id){
  ts_path_obs <- paste(ts_path, "/", subfolder_obs, sep ="")
  file_obs <- paste(observed_data_id, "_SpatialStat_mean.csv", sep ="")

  ts_path_proj <- paste(ts_path, "/", subfolder_proj, sep ="")
  file_proj <- paste(projection_data_id, "_SpatialStat_mean.csv", sep ="")
  #
  # Read the observed data
  obs.df <- file.path(ts_path_obs, file_obs) %>%
    data.table::fread(
      data.table = FALSE,
      header = FALSE,
      col.names = c("year", "month", "obs"),
      showProgress = FALSE)
  #
  # Read the projection data
  #
  # First read the column names - ie the names of the model runs
  #    - there is currently a bug in the BR code 
  #      so I've created COLS_SpatialStat_pr_tas.txt as a work-around
  file_runnames <- if_else(projection_data_id %in% c("pr", "tas"), "COLS_SpatialStat_pr_tas.txt",
                           "COLS_SpatialStat.txt") 
  run_names <- file.path(ts_path_proj, file_runnames) %>%
    data.table::fread(
      data.table = FALSE,
      header = FALSE,
      stringsAsFactors = FALSE,
      na.strings = c("NA", "--", ""),
      showProgress = FALSE)
  run_names <- run_names[,1]
  run_names <- c("year", "month", run_names)
  #
  # Next read the csv file with the input time series of the projections
  proj.df <- file.path(ts_path_proj, file_proj) %>%
    data.table::fread(
      data.table = FALSE,
      header = FALSE,
      col.names = run_names,
      showProgress = FALSE)
  #
  # Combine the obs and projected data
  met.wide.df <- right_join(obs.df, proj.df, by = c("year", "month"))
  #
  # Switch to long format and add column for datatype
  met.df <- met.wide.df %>%
    tidyr::gather(key = "run", value = "val", convert = TRUE, -year, -month)
  met.df$val <- as.numeric(met.df$val) # don't know why this was char
  met.df <- met.df %>%
    mutate(type = datatype)
  return(met.df)
}