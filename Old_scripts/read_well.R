# File to read in the groundwater well data
# function is different from Flume Hobo function

# Functions to read in all the water and well data

require(tidyverse)
require(lubridate)

read_hobo_well <- function(filename, input_dir , 
                      coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                      skip = 1, plotit = F) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  
  colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, °C",
                                "Bar Pressure kPa",
                                "Water Level, meters")
  file_read <- file_read %>%
    mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
  
  file_out <- file_read %>%
    select(`Date and Time`,`Abs Pressure kPa`, `Temp, °C`,
                                `Water Level, meters` )
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Abs Pressure kPa`:`Water Level, meters`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
    print(p)
  }
  return(file_out)
}

# # testing
# read_dir <- "SampleFiles/Wells/Automatic"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test_well <- read_hobo_well(filename = filenames[1], input_dir = read_dir,
#                   plotit = T) 
# head(test_well)