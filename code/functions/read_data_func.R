#******************************************************************
# A function that reads the downloaded data
#   - currently for monthly time series data files
#******************************************************************
# Inputs
#******************************************************************
# ts_path - the relative path to the folder containing run output
# subname - the subdirectory with the data, e.g. bscd5 or 1_8obs
# datatype - type of data, e.g. 
#    pr - precip in mm from projections
#    tas - average temperature in deg C from projections
#    tasmin - average temperature in deg C from projections
#    tasmax - average temperature in deg C from projections
#    prcp - precip in mm from gridded data source
#******************************************************************
# Output
#******************************************************************
# A dataframe with columns: year, month, subname_1, subname_2, ...
#******************************************************************
#
read_data_func <- function(ts_path, subname, datatype){
  ts_path <- paste(ts_path, "/", subname, sep ="")
  print(ts_path)
  # Read the column names
  namefile <- if_else(datatype %in% c("pr", "tas"), "COLS_SpatialStat_pr_tas.txt",
                      "COLS_SpatialStat.txt")
  print(namefile)
  run_names <- file.path(ts_path, namefile) %>%
    data.table::fread(
      data.table = FALSE,
      header = FALSE,
      stringsAsFactors = FALSE,
      na.strings = c("NA", "--", ""),
      showProgress = FALSE)
  run_names <- run_names[,1]
  print(str(run_names))
  run_names <- c("year", "month", run_names)
  print(run_names[1:5])
  #
  # Read the csv file with the input time series
  filename <- paste(datatype, "_SpatialStat_mean.csv", sep = "")
  met.df <- file.path(ts_path, filename) %>%
    data.table::fread(
      data.table = FALSE,
      header = FALSE,
      col.names = run_names,
      showProgress = FALSE)
  #
  return(met.df)
}