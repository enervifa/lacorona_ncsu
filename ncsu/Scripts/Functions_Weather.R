## Functions to read and analyse La Corona weather files

require(tidyverse)

##########################################################
#' Extract the date from a file name
#' 
#' function to extract date from the filename
#' This function only works for the current known filename constructs
#' will need to be checked if construct of file names changes
#' @param file_name character string, name of the file
#' @param split regular expression (regex()) to locate date
#' @example
#'  date <- extract_date("v3p110119.csv")
#'  date
#'  date2 <- extract_date("S1110119.csv")
#'  date2
#' 
#' @export
extract_date <- function(file_name, split = regex("(\\d+)(?!.*\\d)")) {
  locations <- str_locate(file_name, split)
  split_result <- str_sub(file_name, locations[2]-5, locations[2])
  # convert to date
  date <- mdy(split_result)
  # return both
  return(list(date,split_result))
}

##########################################################
#'create date_time
#' 
#' function to create a "Date and Time" column
#' This function assumes that the original day variable is a Julian date
#' @param df_in character string, name of the file
#' @param year_in year value extracted from the file name
#' @example
#' 
#' @export

create_date_time<- function(df_in, year_in) {
  df_out <- df_in %>%
    mutate(time = Add_zero(time)) %>%
    mutate(`Date and Time` = parse_date_time(paste0(year_in,"-",julian," ",time),
                                             "%Y-%j %H%M",
                                             tz = "America/Argentina/Buenos_Aires")) 
  return(df_out)
}


##########################################################
#' auxillary function to fix the dropped 0's in the time values
#' @param x time value that has dropped initial zeros
#' @example
#' test <- c("30","115","1200")
#' Add_zero(test)
#' 
#' @export
Add_zero <- function(x) {
  out <- ifelse(nchar(x) < 3, paste0("00",x),
                ifelse(nchar(x) < 4,paste0("0",x),x))
  return(out)
}

##########################################################
#' plot the data 
#' 
#' function to pivot the variable columns and plot
#' This function assumes that there is a column 'Date and Time'
#' @param df_in character string, name of the file
#' @example
#' 
#' @export
plot_data <- function(df_in) {
  p <- df_in %>%
    pivot_longer(cols = !contains('Date and Time'),
                 names_to = "Measures", values_to ="values") %>%
    ggplot(aes(`Date and Time`,values, colour = Measures)) +
    geom_line() + facet_wrap(~Measures, scales = "free") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45))
  return(p)
}

##########################################################
#' write the processed data to a file 
#' 
#' function to write the processed weather data to a file
#' This function assumes that there is a column 'Date and Time'
#' @param df_in character string, name of the file
#' @param file_name original file name
#' @param output_path directory to write the file to, i.e. paste0(dir, "/processed")
#' @example
#' 
#' @export
write_weather_file <- function(df_in, file_name, output_path) {
  write_csv(df_in,file = paste0(output_path, 
                                "/processed_", file_name), 
            quote = "none")
}


# file has no column names
# c_names <- c("id", "julian", "time", "battery voltage", "Temperature °C", "RelH", 
#              "windspeed", "wind direction", "net radiation", "solar radiation", 
#              "rainfall")

######################################
#' Read in the weather station files
#' 
#' Main function to read in the hobo weather station data files
#' File has no column names
#' @param filename Name of file to read in
#' @param dir directory path for file to read in
#' @param cols_to_drop names of the columns to drop after date time conversion
#' @param c_names_in column names, defaults to vector c_names
#' @param coltypes vector of column types, default given
#' @param plotit flag to plot data, default = F
#' @param writeit flag to write data to disk, default = T
#' @example
#' 
#' @export

read_hobo_weather <- function(filename, dir, cols_to_drop, c_names_in = c_names,
                              coltypes = cols("d","c","c","d","d","d","d","d","d","d","d"),
                              plotit = F, write_it = T) {
  #browser()
  file_read <- read_csv(paste(dir,filename,sep="/"),
                        col_names=c_names_in, col_types = coltypes)
  
  #the year is not stated in the file, interpret from filename
  year_file <- year(extract_date(filename)[[1]])
  # Create a 'Date and Time' column
  file_out <- create_date_time(file_read,year_file) %>%
    select(!cols_to_drop)

  if (write_it == T) {
    write_weather_file(file_out, filename, paste0(dir,"/processed"))
  }
  
  # plotting        
  if (plotit == T) {
    p <- plot_data(file_out)
    return(list(data = file_out, plot = p)) 
  } else return(file_out) 
  
 
}          


daily_summary <- function(df_in) {
  daily_minmax <- df_in  %>%
    group_by(Date = lubridate::date(`Date and Time`)) %>%
    summarise(
      Tmax = max(`Temperature °C`),
      Tmin = min(`Temperature °C`),
      Tmean= ((Tmax + Tmin)/2),
      RHmax=max(RelH),
      RHmin=min(RelH),
      RHmean=((RHmax + RHmin)/2),
      WS=mean(windspeed),
      SRsum=sum(`solar radiation`),
      SR=((sum(`solar radiation`)/(15*60*1000000))),
      NSR_calc=(SR*(1-0.23)*3600*24),
    )
  return(daily_minmax)
}


calculate_E0 <- function(Temp_in) {
  E0_out <- 0.6108*exp((17.27*Temp_in)/(Temp_in+237.3))
}

calculate_Et0 <- function(df_minmax) {
  daily_all<- df_minmax %>%
    mutate(Slope_vap_pres=(4098*((calculate_E0(Tmean))/(Tmean + 237.3)^2)))%>%
    mutate(GSHFD = 0, #all 0 from Chip excel
           # calculate pressure
           P =101.3*(((293-0.0065*170)/293)^5.26),#170 z station level (m)
           # calculate psychrometric constant
           psyc_con=P*0.000664742,
           #E0 values based on Tmax, Tmin and Tmean
           E0tmax=calculate_E0(Tmax),
           E0tmin=calculate_E0(Tmin),
           E0tmean=calculate_E0(Tmean),
           # calculate ES and EA
           ES=(E0tmax+E0tmin)/2,
           EA=(E0tmin*RHmax/100 + E0tmax*RHmin/100)/2,
           # calculate E0
           ET0=(0.408*Slope_vap_pres*(NSR_calc - GSHFD)+
                  psyc_con*(900/(Tmean + 273))* WS*
                  (ES-EA))/(Slope_vap_pres + psyc_con*(1+0.34*WS)))
  
  return(daily_all)
}


# Read in the weather station files
ET0_hobo_weather <- function(filename, dir , cols_to_drop, c_names_in = c_names,
                             coltypes = cols("d","c","c","d","d","d","d","d","d","d","d"),
                             plot_data = F) {
  
  file_out <- read_hobo_weather(filename, dir, cols_to_drop, write_it=F, plotit=F)
  # #browser()
  # file_read <- read_csv(paste(dir,filename,sep="/"),
  #                       col_names=c_names, col_types = coltypes)
  # 
  # #the year is not stated in the file, interpret from filename
  # year_file <- year(extract_date(filename)[[1]])
  # # Create a 'Date and Time' column
  # file_out <- create_date_time(file_read,year_file)
  # # Extract Tmax and Tmin per day
  # 
  # file_out2 <- file_out%>%
  #   #mutate(Date=make_date(year(`Date and Time`),month(`Date and Time`),day(`Date and Time`)))%>%
  #   mutate(date=floor_date(`Date and Time`, unit = "day"))%>%
  #   mutate(dia=day(date))
  # 
  # calculate daily minimum and maxima
  daily_minmax <- daily_summary(file_out)

  # calculate all daily values and ET0
  daily_all <- calculate_Et0(daily_minmax)
  
  
  #select important columns
  daily_ET0 <-  daily_all%>%
    mutate(ET0r=round(ET0,3))%>%
    select(Date,ET0r)
  
  #save file with ETO
  write_csv(daily_ET0, file = paste0(dir,"/processed/ET0daily", filename), 
            quote = "none")
  #browser()    
  
  if (plot_data == T) {
    
    ETD<- daily_all %>%
      select(c(Date, ET0)) %>%
      ggplot(aes(Date,ET0)) + geom_line(colour = "blue") + theme_bw()
    return(list(data = daily_all, plot = ETD))
  #  print(ggplotly(ETD))
    
  } else   return(daily_all)
  cat("ET0 calculated and saved")

  
}        
