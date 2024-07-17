# read weather files La Corona

require(tidyverse)
require(lubridate)


# file has no column names
c_names <- c("id", "julian", "time", "battery voltage", "Temperature °C", "RelH", 
              "windspeed", "wind direction", "net radiation", "solar radiation", 
              "rainfall")


# Read in the weather station files
read_hobo_weather <- function(filename, dir ,
                         coltypes = cols("d","c","c","d","d","d","d","d","d","d","d"),
                           plotit = F) {
#browser()
  file_read <- read_csv(paste(dir,filename,sep="/"),
                        col_names=c_names, col_types = coltypes)
  
  #the year is not stated in the file, interpret from filename
  year_file <- paste0("20",substr(filename,
                                  (nchar(filename)-5),
                                  (nchar(filename)-4)))
  
  file_out <- file_read %>%
    mutate(time = Add_zero(time)) %>%
    mutate(`Date and Time` = parse_date_time(paste0(year_file,"-",julian," ",time),
                                             "%Y-%j %H%M",
                                             tz = "America/Argentina/Buenos_Aires")) 

  #browser()        
  if (plotit == T) {
    p <- file_out %>%
      pivot_longer(cols = `Temperature °C`:rainfall,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, scales = "free")
    print(p)
  }
  return(file_out)
}          

# # testing read_weather
# read_dir <- "SampleFiles/Weather"
# filename <- dir(path = read_dir, pattern = ".dat")
# 
# test_weather<- read_weather(filename[1],dir = read_dir,
#                                 plotit = T) 
# 


# auxillary function to fix the dropped 0's in the time values

Add_zero <- function(x) {
  out <- ifelse(nchar(x) < 3, paste0("00",x),
                ifelse(nchar(x) < 4,paste0("0",x),x))
  return(out)
}
## testing
# test <- c("30","115","1200")
# Add_zero(test)