#******************************************************************
# A function that reads the downloaded data
#   - currently for monthly time series data files
#******************************************************************
# Inputs
#******************************************************************
# ts_path - the relative path to the folder containing runs
# subname - the subfolder name, e.g. rcp26
# filename - the file to be read, e.g. pr_SpatialStat_mean.csv
#******************************************************************
# Output
#******************************************************************
# A dataframe with columns: year, month, subname_1, subname_2, ...
#******************************************************************
#
read_data_func <- function(ts_path, subname, filename){
  d_reduction_factor <- 1.0 - dr_wma
  # This is an demand multiplication factor set in parameters.R
  #   - it's usually 1 but can be something else for QAing or other purposes
  d_factor <- d_reduction_factor*d_wma_factor
  #
  demands.fc.df <- demands.daily.df %>%
    dplyr::filter(date_time >= date_sim000,
                  date_time < date_sim000 + 15) %>%
    dplyr::mutate(d_fw_e = d_fw_e*d_factor,
                  d_fw_w = d_fw_w*d_factor,
                  d_fw_c = d_fw_c*d_factor,
                  d_lw = d_lw*d_factor,
                  d_wssc = d_wssc*d_factor,
                  d_wa = d_wa*d_factor,
                  d_total = d_total*d_factor) %>%
    dplyr::select(date_time, d_fw_e, d_fw_w, d_fw_c, d_lw,
                  d_wa, d_wssc)
  return(demands.fc.df)
}