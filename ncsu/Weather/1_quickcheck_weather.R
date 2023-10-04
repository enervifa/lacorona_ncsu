
# read weather files La Corona

require(tidyverse)
require(lubridate)

# # testing read_weather
# read_dir <- "SampleFiles/Weather"
# read_dir  <- "C:/Users/Lenovo/Downloads/SampleFiles3/SampleFiles_test/T061523/"
# filename <- dir(path = read_dir, pattern = ".dat")
# 
# test_weather<- read_hobo_weather (filename[1],dir = read_dir,
#                                 plotit = T) 

#test_ET0<- ET0_hobo_weather (filename[1],dir = read_dir,
 #                                 plotit = T) 
# 


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
  
  browser()        
  if (plotit == T) {
    p <- file_out %>%
      pivot_longer(cols = `Temperature °C`:rainfall,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, scales = "free")
    print(ggplotly(p))
  }
  return(file_out)
}          


# auxillary function to fix the dropped 0's in the time values

Add_zero <- function(x) {
  out <- ifelse(nchar(x) < 3, paste0("00",x),
                ifelse(nchar(x) < 4,paste0("0",x),x))
  return(out)
}

## testing
# test <- c("30","115","1200")
# Add_zero(test)

# Read in the weather station files
ET0_hobo_weather <- function(filename, dir ,
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
  # Extract Tmax and Tmin per day
  
  file_out2 <- file_out%>%
    #mutate(Date=make_date(year(`Date and Time`),month(`Date and Time`),day(`Date and Time`)))%>%
    mutate(date=floor_date(`Date and Time`, unit = "day"))%>%
    mutate(dia=day(date))
  
  colnames(file_out2)<-c("id","julian","time","battery_voltage","Temp","RelH","windspeed","wind direction",
                          "net_radiation" ,"solar_radiation", "rainfall", "DateandTime","date","dia")
  daily_minmax <- file_out2  %>%
    group_by(date) %>%
    summarise(
      Tmax = max(Temp),
      Tmin = min(Temp),
      Tmean= ((Tmax+Tmin)/2),
      RHmax=max(RelH),
      RHmin=min(RelH),
      RHmean=((RHmax+RHmin)/2),
      WS=mean(windspeed),
      SRsum=sum(solar_radiation),
      SR=((sum(solar_radiation)/(15*60*1000000))),
      NSR_calc=(SR*(1-0.23)*3600*24),
    )
  
  daily_all<- daily_minmax %>%
    mutate(Slope_vap_pres=(4098*((0.6108*exp((17.27 * Tmean)/(Tmean + 237.3)))/(Tmean + 237.3)^2)))%>%
    mutate(GSHFD = 0, #all 0 from Chip excel
    P =101.3*(((293-0.0065*170)/293)^5.26),#170 z station level (m)
    psyc_con=P*0.000664742,
    E0tmax=0.6108*exp((17.27*Tmax)/(Tmax+237.3)),
    E0tmmin=0.6108*exp((17.27*Tmin)/(Tmin+237.3)),
    E0tmean=0.6108*exp((17.27*Tmean)/(Tmean+237.3)),
    ES=(E0tmax+E0tmmin)/2,
    EA=(E0tmmin*RHmax/100+E0tmax*RHmin/100)/2,
    ET0=(0.408*Slope_vap_pres*(NSR_calc-GSHFD)+psyc_con*(900/(Tmean+273))* WS*(ES-EA))/(Slope_vap_pres+psyc_con*(1+0.34*WS)))
  
  #select important columns
  daily_ET0<-   daily_all%>%
    mutate(ET0r=round(ET0,3))%>%
    select(date,ET0r)
  
  #save file with ETO
  write.table(daily_ET0, file = paste0(filename,"ET0daily"), sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
   browser()    
   
  if (plotit == T) {
    
    ETD<- daily_all%>%
      pivot_longer(cols = `ET0`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`date`,values, colour = Measures)) +
      geom_col() + facet_wrap(~Measures, scales = "fixed")
    
    ETDg<-ggplotly(ETD)
    print(ETDg)

  }
   cat("ET0 calculated and saved")
   return(daily_all)

}        



