#read rain function

library(tidyverse)
library(lubridate)
library(ggplot2)
library(hydroTSM)
library(plotly)
require(dygraphs)
require(xts)

# Read in the hobo raingauhge files
read_hobo_rain <- function(filename, dir ,
                           coltypes = cols("d","c","d","d","c","c","c","c"),
                           skip = 1, plotit = F) {
  #browser()
  file_read <- read_csv(paste(dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  file_read <- file_read %>%
    mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
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
    mutate(`Rain, mm` = c(0,diff(`Rain, tip count`)))
  
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


#read rain file

read_dir <- "SampleFiles/Rain/Automatic"
filename <- dir(path = read_dir, pattern = ".csv")

test_hobo_rain<- read_hobo_rain(filename[1],dir = read_dir,
                                plotit = T) 

#first try plotting one dataset

#x <- c(1:100)
#random_y <- rnorm(100, mean = 0)

#data <- data.frame(test_hobo_rain$`Date and Time`, test_hobo_rain$`Rain, tip count`)

# <- plot_ly(test_hobo_rain, x = ~`Date and Time`, y = ~`Rain, tip count`, type = 'scatter', mode = 'lines')

#fig2 <- plot_ly(test_hobo_rain, x = ~`Date and Time`, y = ~cumsum(`Rain, mm`), type = 'scatter', mode = 'lines')


#Plot Rain  by Day (mm)
test<-test_hobo_rain %>% group_by(Day = date(`Date and Time`)) %>%  # day(), date(9, hour(), group by date (entire time of a day) and ignore NAs
  summarise(Rain = sum(`Rain, mm`,na.rm = TRUE)) #summarise make "anything" with data, in this case we add up rain, we can make other calculations SD as example
fig1<-plot_ly(test,x=~Day, y=~Rain, type = 'scatter', mode = 'points')
fig1

#Plot Rain  by hour (mm)

testhour<-test_hobo_rain %>% group_by(Day= date(`Date and Time`), Hour= hour(`Date and Time`))  %>%  # group by Day and by Hour
  summarise(Rain = sum(`Rain, mm`,na.rm = TRUE))%>%
  mutate(Day_hr=ymd_h(paste(Day,Hour)))
 
fig2<-plot_ly(testhour,x=~Day_hr, y=~Rain, type = 'scatter', mode = 'points')
fig2

#group_by(Day = date(`Date and Time`), hour = hour(`Date and Time`)) if I want to have the date and hour
#in summarise we can use other operations sum, mean, ave, multiple operations can be done
#run functions to read flumes data

#Plot Flumes  by Day (mm)

test<-test_hobo_rain %>% group_by(Day = date(`Date and Time`)) %>%  # day(), date(9, hour(), group by date (entire time of a day) and ignore NAs
  summarise(Rain = sum(`Rain, mm`,na.rm = TRUE)) #summarise make "anything" with data, in this case we add up rain, we can make other calculations SD as example
fig1<-plot_ly(test,x=~Day, y=~Rain, type = 'scatter', mode = 'points')
fig1

#create files

#HOBOU20
read_dir2 <- "SampleFiles/Flumes/V1V2/HoboU20OutsideWell"
filenames <- dir(path = read_dir2, pattern = ".csv")

test_flume_hobou <- read_hobo(filename = filenames[1], input_dir = read_dir2,
                  plotit = T) 
head(test)

#Stevens
read_dir3 <- "SampleFiles/Flumes/V1V2/StevensHoboU12"
filenames <- dir(path = read_dir3, pattern = ".csv")

test_stevens <- read_stevens(filenames[1], input_dir = read_dir3,
                             plotit = T)
head(test_stevens)

#ISCO
read_dir4 <- "SampleFiles/Flumes/V1V2/ISCOSampler"
filenames <- dir(path = read_dir4, pattern = ".csv")

test_isco <- read_isco(filename = filenames[1], input_dir = read_dir4,
                       plotit = T) 
head(test_isco)


#first try plotting three datasets

library(plotly)

trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- test

data <- data.frame(zootry$, trace_0, trace_1, trace_2)

fig <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')

fig

#merge data by Date and Time

#summarise by hour all dataframes

test_flume_hobou_av<-test_flume_hobou %>% group_by(Day= date(`Date and Time`), Hour= hour(`Date and Time`))  %>%  # group by Day and by Hour
  summarise(Temp= mean(`Temp, °C`,na.rm = TRUE),
           Level=mean(`Water Level, meters`, na.rm=TRUE) )%>%
  mutate(Day_hr=ymd_h(paste(Day,Hour)))


test_flume_hobou_av<-test_flume_hobou %>% group_by(Day= date(`Date and Time`), Hour= hour(`Date and Time`))  %>%  # group by Day and by Hour
  summarise(Temp= mean(`Temp, °C`,na.rm = TRUE),
            Level=mean(`Water Level, meters`, na.rm=TRUE) )%>%
  mutate(Day_hr=ymd_h(paste(Day,Hour)))

#Join two datasets before plotting
jointry<- as.data.frame(full_join(test_flume_hobou_av,testhour, by= "Day_hr"))

figjoined<- plot_ly(jointry, x = ~Day_hr, y = ~Temp, name = 'Temperature °C', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~Level, name = 'Water Level (ft)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Rain, name = 'Rainfall (mm))', type = 'scatter', mode = 'lines') 


jointry<- left_join()
pivot_longer()