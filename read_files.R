# Reading in la Corona data

# Functions to read in all the water and well data

require(tidyverse)
require(lubridate)

# --- HOBOU20 ----

read_hobou20 <- function(filename, input_dir ,
                          coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                          skip = 1, plotit = F) {
#browser()
      file_read <- read_csv(paste(input_dir,filename,sep="/"),
                            skip = skip, col_types = coltypes)
      file_read <- file_read %>%
        mutate(`Date and Time` = force_tz(mdy_hms(`Date Time, GMT-03:00`),
                                         tz = "America/Argentina/Buenos_Aires")) 
      colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, °C",
                                    "Bar Pressure kPa",
                                    "Water Level, meters")
      file_out <- file_read %>%
        select(`Date and Time`, `Temp, °C`,
               `Water Level, meters`)
    
      if (plotit == T) {
        p <- file_out %>%
          na.omit() %>%
          pivot_longer(cols = `Temp, °C`:`Water Level, meters`,
                       names_to = "Measures", values_to ="values") %>%
          ggplot(aes(`Date and Time`,values, colour = Measures)) +
            geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
        print(p)
      }
  return(file_out %>% select(`Date and Time`,
                             `Water Level, meters`))
}

# # testing
# read_dir <- "SampleFiles/Flumes/V1V2/HoboU20OutsideWell"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test <- read_hobou20(filename = filenames[1], input_dir = read_dir,
#                       plotit = T) 
# head(test)

# --- HOBOU12--- Stevens

# read the stevens logger
read_stevens <- function(filename, input_dir,
                         skip = 1, plotit = F) {
  #browser()
  # messy, but currently the only way I can do this
  if(grepl("V1",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c","c","c")
  if(grepl("V2",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c")
  if(grepl("V3",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","c","c","c","c","c")
  if(grepl("V4",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","c","c","c")
  
  
  
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  file_read <- file_read %>%
    mutate(`Date and Time` = force_tz(mdy_hms(`Date Time, GMT-03:00`),
                                     tz = "America/Argentina/Buenos_Aires")) 
  colnames(file_read)[3] <- "Volt, V"
  # if (grep("Temp, °C", colnames(file_read)[4]) == T) {
  #   colnames(file_read)[4] <- "Temp, °C"
  #   file_out <- file_read %>%
  #     select(`Date and Time`, `Volt, V`,`Temp, °C`)
  # } else {
    file_out <- file_read %>%
      select(`Date and Time`, `Volt, V`)
#  }
  
  if (plotit == T) {
    p <- file_out %>%
      ggplot(aes(`Date and Time`,`Volt, V`)) + geom_line() +
      theme_bw()
    print(p)
  }
  return(file_out %>% select(`Date and Time`, `Volt, V`))
}

# testing
# read_dir <- "SampleFiles/Flumes/V1V2/StevensHoboU12"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test_stevens <- read_stevens(filenames[2], input_dir = read_dir,
#                   plotit = T)
# head(test_stevens)

# --- ISCO ----
read_isco <- function(filename, input_dir , 
                      coltypes = cols("c","i","i"),
                      skip = 7, plotit = F) {
#browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        col_names =F,
                        skip = skip, col_types = coltypes)
  colnames(file_read) <- c("Date and Time", "Sample",
                                "Level (ft)")
  file_out <- file_read %>%
    mutate(`Date and Time` = time_convert(`Date and Time`)) %>%
    mutate(`Level (ft)` = as.numeric(paste(Sample, `Level (ft)`, sep = ".")))

  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Sample`:`Level (ft)`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
    print(p)
  }
  return(file_out %>% select(`Date and Time`,`Level (ft)`))
}

# # testing
# read_dir <- "SampleFiles/Flumes/V1V2/ISCOSampler"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test_isco <- read_isco(filename = filenames[1], input_dir = read_dir,
#                   plotit = T)
# head(test_isco)

# --- ISCO velocity ----
read_isco_velocity <- function(filename, input_dir , 
                      coltypes = cols("c","i","i"),
                      skip = 7, plotit = F) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        col_names =F,
                        skip = skip, col_types = coltypes)
  colnames(file_read) <- c("Date and Time", "Sample",
                           "velocity (ft/s)")
  file_out <- file_read %>%
    mutate(`Date and Time` = time_convert(`Date and Time`)) %>%
    mutate(`velocity (ft/s)` = as.numeric(paste(Sample, `velocity (ft/s)`, sep = ".")))
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Sample`:`velocity (ft/s)`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
    print(p)
  }
  return(file_out %>% select(`Date and Time`,`velocity (ft/s)`))
}



## Barometric pressure (V3/V4)

read_bar <- function(filename, input_dir ,
                      coltypes = cols("d","c","d","d","c","c","c","c","c","c"),
                      skip = 1, plotit = F) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  file_read <- file_read %>%
    mutate(`Date and Time` = force_tz(mdy_hms(`Date Time, GMT-03:00`), 
                                     tz = "America/Argentina/Buenos_Aires")) 
  colnames(file_read)[3:4] <- c("Abs Pressure kPa", "Temp, °C")
  file_out <- file_read %>%
    select(`Date and Time`,`Abs Pressure kPa`, `Temp, °C`)
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Abs Pressure kPa`:`Temp, °C`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
    print(p)
  }
  return(file_out)
}

# # testing
# read_dir <- "SampleFiles/Flumes/V3V4/BarometricPressure"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test_bar <- read_bar(filename = filenames[1], input_dir = read_dir,
#                   plotit = T) 
# head(test_bar)
# 



# auxillary function to deal with a.m. and p.m.

time_convert <- function(text_input) {
  text_out <- str_sub(text_input,1,19)
  text_out <- ifelse(str_detect(text_input, "a.m.") == T,
    str_c(text_out," AM"),
    str_c(text_out," PM"))
  text_out <- parse_date_time(text_out,
                              "dmy IMS Op",
                              tz = "America/Argentina/Buenos_Aires")
  return(text_out)
}
  

