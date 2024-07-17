# File to read in the groundwater well data
# function is different from Flume Hobo function

# Functions to read in all the water and well data

#require(tidyverse)
#require(lubridate)

##################################################
#' Read well file
#' 
#' Function to read well file
#' @param filename the file name to read in
#' @param input_dir any additional path, defaults to "."
#' @param coltypes column types, has a default to match current files
#' @param skip defaults to 1
#' @param plotit Switch whether or not to plot the output
#' @example 
#' df1 <- read_hobo_well(filename = "N1111219.csv", 
#'                    input_dir = "../Wells/Automatic")
#' 
#' @export
read_hobo_well <- function(filename, input_dir = "."  , 
                      coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                      skip = 1, plotit = F) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  
  colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, ?C",
                                "Bar Pressure kPa",
                                "Water Level, meters")
  file_read <- file_read %>%
    mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
  
  file_out <- file_read %>%
    select(`Date and Time`,`Abs Pressure kPa`, `Temp, ?C`,
                                `Water Level, meters` ) %>%
    # remove missing values
    na.omit() %>%
    # remove first two hours as these are often wrong
    filter(`Date and Time` > min(`Date and Time`) + 2)
  
  
  #write_well(file_out,filename)
  
  if (plotit == T) {
    p <- plot_well(file_out)
    #print(p)
  } else p <- NA
  return(list(file = file_out, plot = p))
}


#######################################
#' plotting function
#' 
#' Function to plot the well output
#' @param df dataframe to plot
#' @param baro switch (true/false) for baro fiel plotting
#' @example
#' 
plot_well <- function(df, baro = FALSE) {
  if (baro  == F) {
    p <- df %>%
      na.omit() %>%
      pivot_longer(cols = 2:ncol(df),
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free") + 
      theme_bw()
    
  } else {
    p <- df %>%
      na.omit() %>%
      ggplot(aes(`Date and Time`,`Barom Pressure kPa`)) +
      geom_line(colour = "blue") + theme_bw()
  }
  return(p)
}

##################################
#' Write processed file
#' 
#' Write the processed file to a path
#' @param df dataframe to write
#' @param filename_in filename to write processed
#' @param path_out directory to write the file
#' @example
write_well <- function(df, filename_in, path_out = "../Wells/Automatic/Processed") {
  write_csv(df, paste0(path_out,"/",filename_in,"_processed.csv"))
}

##################################################
#' Read baro file
#' 
#' Function to read the baro file
#' @param filename the file name to read in
#' @param input_dir any additional path, defaults to "."
#' @param coltypes column types, has a default to match current baro file
#' @param skip defaults to 1
#' 
#' @example 
#' df1 <- read_hobo_baro(filename = "N11111219.csv", 
#'                    input_dir = "../Wells/Automatic")
#' 
#' @export
read_hobo_baro <- function(filename, input_dir = "."  , 
                           coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                           skip = 1, plotit = T) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  
  colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, ?C",
                                "Barom Pressure kPa",
                                "Water Level, meters")
  file_read <- file_read %>%
    mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
  
  file_out <- file_read %>%
    select(`Date and Time`,`Barom Pressure kPa`)

  if (plotit == T) {
    p <- plot_well(file_out, baro = T)
    #print(p)
  } else p <- NA
  return(list(file = file_out, plot = p))
}

#############################
#' merge baro and well at hourly time steps
#' 
#' Merge the baro pressure and well data to process
#' @param baro_df file with baro pressure data
#' @param well_df file with waterlevel/pressure data
#' @example 
#' 
Merge_baro_well <- function(baro_df, well_df) {
  # First we need to summarise the barometric pressure over the hour that the
  # well measurements are taken
 # browser()
  baro_df_test <- baro_df %>%
    mutate(Datehour = round(`Date and Time`, unit = 'hour')) %>%
    group_by(DateTime = Datehour) %>%
    summarise(`Barom Pressure kPa` = mean(`Barom Pressure kPa`, na.rm =T))
  print(baro_df_test)
  
  merge_dfs <- left_join(baro_df_test, well_df %>% 
                           mutate(DateTime = round(`Date and Time`, 
                                                   unit = 'hour')))
  # make a quick plot
  p <- merge_dfs %>% ggplot(aes(`Barom Pressure kPa`, `Abs Pressure kPa`)) + 
    geom_point() + geom_smooth(method = "lm") + theme_bw()
  
  return(list(data = merge_dfs, plot = p))
  
}

#############################
#' read manual well
#' 
#' read the manual well files
#' @param filename the file name to read in
#' @param input_dir any additional path, defaults to "."
#' @example
#' 
read_manual_well <- function(filename, input_dir = "../Wells/Manual") {
 # foo <- file(paste(input_dir, filename, sep ="/"))
  data <- read_docx(paste(input_dir, filename, sep ="/"))
#  close(foo)
browser()  
line_well <- grep("Well", data) 

  
}



# # testing
# read_dir <- "SampleFiles/Wells/Automatic"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test_well <- read_hobo_well(filename = filenames[1], input_dir = read_dir,
#                   plotit = T) 
# head(test_well)