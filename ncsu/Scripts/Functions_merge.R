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
read_flume_output2 <- function(name, path_to_file="."){
  #force date column to be character
  data_out <- read_csv(paste0(path_to_file,"/",name), col_types = cols("c","d"))
    # make sure dates are working
  #browser()
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
   #browser()
    # add zero rainfall times
    new_date_time <- tibble(sum_time = seq(new_df$sum_time[1],new_df$sum_time[nrow(new_df)], 
                                           by = timestep),
                            Event = 0)
    new_df_summary <- left_join(new_date_time,new_df_summary, by = "sum_time") %>%
      mutate(value = ifelse(is.na(value) == T, Event, value)) %>%
      select(sum_time,value)
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

##################################################
#' Merge two data frames with logger data 
#' 
#' Function to merge two data frames where one can overlap in dates
#' data frames with logger data for two different periods
#' @param P1_df input data frame for period 1
#' @param P2_df input data frame for period 2
#' @example 
#' merged_df <- merge_periods(Period1_file, Period2_file)
#' 
#' @export
merge_periods <- function(P1_df, P2_df) {
  # find the date ranges for both data frames
  P1_range <- range(P1_df$`Date and Time`, na.rm =T)
  P2_range <- range(P2_df$`Date and Time`, na.rm =T)
  # check if 1) first date P1 later than first date P2 and 
  # 2) the range of dates from P1 is shorter than P2
  if (P1_range[1] > P2_range[1] & 
      (P1_range[2] - P1_range[1]) < (P2_range[2] - P2_range[1]) ){
    SecA <- P2_df %>% filter(`Date and Time` < P1_range[1])
    SecB <- P2_df %>% filter(`Date and Time` >= P1_range[1] &
                                      `Date and Time` <= P1_range[2]) %>%
      mutate(across(contains("Water Level"), 
                    ~ coalesce(., P1_df[[cur_column()]]))) %>%
      mutate(across(contains("Event"), 
                    ~ coalesce(., P1_df[[cur_column()]])))
    SecC <- P2_df %>% filter(`Date and Time` >= P1_range[2])
    combined_p <- bind_rows(SecA, SecB, SecC)
    
  } else {
    # if P1 and P2 do not overlap
    if (P1_range[1] < P2_range[1]) {
      # simple bind_rows
      combined_p <- bind_rows(Period1_file,Period2_file)
    } else {
      # if they overlap, but not one file longer than other
      combined_p <- left_join(Period1_file,Period2_file)
    }
  }
  return(combined_p)
}

##################################################
#' Make a plot of a merged file
#' 
#' Function to create a ggplot object for a merged data file 
#' with multiple logger columns
#' @param df input data 
#' @example 
#' p <- plot_merged(merged_df)
#' 
#' @export
plot_merged <- function(df) {
  p_out <- df %>%
    pivot_longer(3:ncol(df),values_to = "value", 
                 names_to="variable") %>%
    ggplot(aes(`Date and Time`, Event*0.2)) + 
    geom_point(colour = "blue", alpha = 0.5) +
    #geom_bar(stat="identity", fill = "darkblue") + 
    geom_line(aes(`Date and Time`,value, colour =variable)) + 
    theme_bw() + ylab("Rainfall (mm)") + 
    scale_y_continuous(sec.axis = sec_axis(trans=~., name = "water level (m)"))
  return(p_out)
  
}