# Reading in la Corona data

# Functions to read in all the Rainfall data


require(tidyverse)
require(lubridate)

# Read in the hobo raingauge files
read_hobo_rain <- function(filename, dir ,
                          #coltypes = cols("d","c","d","d","c","c","c","c"),
                          skip = 1, plotit = F) {
#browser()
  # messy, but currently the only way I can do this
  if(grepl("R1",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c","c")
  if(grepl("R4",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c")
  
      file_read <- read_csv(paste(dir,filename,sep="/"),
                            skip = skip, col_types = coltypes)
      file_read <- file_read %>%
      mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`,
                                       tz = "America/Argentina/Buenos_Aires")) 
      colnames(file_read)[3:4] <- c("Temp, °C", "Rain, tip count")
      #browser()
      # split out the temperature data
      file_out_temp <- file_read %>%
        select(`Date and Time`,`Temp, °C`) %>%
        # remove NA values
        na.omit()
      # split out the rainfall tips
      file_out_rain <- file_read %>%
          select(`Date and Time`,`Rain, tip count`) %>%
    # remove NA values
        na.omit() %>%
    # calculate the difference
        mutate(`Rain, mm` = c(0,diff(`Rain, tip count`))*0.0254) 
      
    # join files back together (needs full_join)
      file_out <- full_join(file_out_temp,file_out_rain)
              #browser()        
      if (plotit == T) {
        p <- file_out %>%
          pivot_longer(cols = c(`Temp, °C`,`Rain, mm`),
                       names_to = "Measures", values_to ="values") %>%
          ggplot(aes(`Date and Time`,values, colour = Measures)) +
          geom_line(size =2) + facet_wrap(~Measures, ncol = 2, scales = "free")
        print(p)
      }
return(file_out)
}



# #Alternative with bind_rows() but I think the issue here is as the colnames does not match it adds  NA when bindinh an it is not correct
# # I tried na.exclude() to get diff accurately, but still struggling
# 
#           read_hobo_rain2 <- function(filename, dir ,
#                                      coltypes = cols("d","c","d","d","c","c","c","c"),
#                                      skip = 1, plotit = F) {
#             browser()
#             file_read <- read_csv(paste(dir,filename,sep="/"),
#                                   skip = skip, col_types = coltypes)
#             file_read <- file_read %>%
#               mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
#             colnames(file_read)[3:4] <- c("Temp, °C", "Rain, tip count")
#             #browser()
#             file_out_rain <- file_read %>%
#               select(`Date and Time`,`Rain, tip count`)
#             file_out_rain2=file_out_rain %>%
#             na.exclude(file_out_rain2) %>%
#               #file_out_rain2[is.na(file_out_rain2)] = 0
#               #file_out_rain_m<- file_out_rain2 %>%
#               mutate(`Rain, mm` = c(0,diff(`Rain, tip count`))) 
#             #file_out_rain_m$`Rain, mm`[file_out_rain_m$`Rain, mm` <= 0] = NA
#             file_out_temp <- file_read %>%
#               select(`Date and Time`, `Temp, °C`)
#             file_out<-bind_rows(file_out_temp ,file_out_rain2)
#             
#             #browser()        
#             if (plotit == T) {
#               p <- file_out %>%
#                 #na.omit(file_out_rain_m$`Rain, mm`) %>%
#                 pivot_longer(cols = `Temp, °C`:`Rain, mm`,
#                              names_to = "Measures", values_to ="values") %>%
#                 ggplot(aes(`Date and Time`,values, colour = Measures)) +
#                 geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
#               print(p)
#             }
#             return(file_out)
#           }
# 
          
          # # testing read_hobo_rain
          # read_dir <- "SampleFiles/Rain/Automatic"
          # filename <- dir(path = read_dir, pattern = ".csv")
          # 
          # test_hobo_rain<- read_hobo_rain(filename[1],dir = read_dir,
          #                                  plotit = T) 
          # 
          # 
          # 
          # # testing read_hobo_rain2
          # 
          # test_hobo_rain<- read_hobo_rain2(filename[1],dir = read_dir,
          #                                  plotit = T) 
          # 
          
          