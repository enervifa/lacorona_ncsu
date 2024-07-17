# Functions to combine data from different instruments
# flumes, weather, rainfall
# summarise to 6 minute, daily, monthly and annual

##########################################################
#' Joining data frames by date
#' 
#' function to join two data sets (from different instruments)
#' in this case the first data frame will be the dates and times to match
#' This presumes that you know that the dates in the files overlap
#' This also assumes thet both files have the same name for the date and time column
#' @param df1 name of the first data frame
#' @param df2 name of the second data frame
#' @param by_cols names of columns to merge
#' @example
#'  
#' 
#' @export
#' 
merge_data <- function(df1, df2, by_cols = NULL) {
  df_out <- full_join(df1,df2, by = c(by_cols))
  return(df_out)
}

#################################################
#' read otip file
#' 
#' This reads the "tips" file from the raingauge data
#' @param name the file name to read in
#' @aram path_to_file any additional path, defaults to "."
#' @example 
#' df1 <- read_otip_file(name = "OTIP_r4111219.csv", 
#'                    path_to_file = "../Rain/processed")
#' 
#' @export
read_otip_file <- function(name, path_to_file="."){
  data_out <- read_csv(paste0(path_to_file,"/",name))
  # make sure dates are working
  data_out <- data_out %>%
    mutate(`Date and Time` = ymd_hms(Date, tz = "America/Argentina/Buenos_Aires")) %>%
    # rename Difference to Event
    rename("Event" = "Difference") %>%
    # and reorganise
    select(-Date) %>%
    select(`Date and Time`, Event)
  return(data_out)
}

##################################################
#' Read processed flume file
#' 
#' Function to read the processed flume file back into a dataframe
#' @param name the file name to read in
#' @param path_to_file any additional path, defaults to "."
#' @example 
#' df1 <- read_flume_output(name = "S1110119_processed.csv", 
#'                    path_to_file = "../Flumes/processed")
#' 
#' @export
read_flume_output <- function(name, path_to_file="."){
  data_out <- read_csv(paste0(path_to_file,"/",name))
  # make sure dates are working
  data_out <- data_out %>%
    mutate(`Date and Time` = ymd_hms(`Date and Time`, tz = "America/Argentina/Buenos_Aires"))
  return(data_out)
}


##################################################
#' Summarise to 6 min, daily or monthly
#' 
#' Function to summarise processed data to standard 6 minute, daily or monthly
#' starting from midnight
#' @param df input data frame
#' @param timestep summarising time step character input: "6 min", "day", "month"
#' @example 
#' summarised <- sum_fun(df_rain, timestep = "6 min")
#' 
#' @export
sum_fun <- function(df, timestep = "6 min") {
  # define sequence of time points to summarise
  new_df <- df %>%
    mutate(sum_time = floor_date(df$`Date and Time`, timestep))
  # summarise on  times
  # browser()
  summary_column <- as.character(names(new_df[2]))
  # cheating, should be done more elegantly
  if (any(grepl("Event", names(new_df)))) {
    new_df_summary <- new_df %>%
      group_by(sum_time) %>%
      summarise(value = sum(!!sym(summary_column), na.rm = T))
  } else {
    new_df_summary <- new_df %>%
      group_by(sum_time) %>%
      summarise(value = mean(!!sym(summary_column), na.rm = T))
  }
  # rename the columns
  new_df_summary <- new_df_summary %>%
    rename("Date and Time" = "sum_time") 
  new_df_summary <- new_df_summary %>%
    rename(!!summary_column := value)
    #rename(.dots = setNames("value",!!summary_column))  
  return(new_df_summary)
}

