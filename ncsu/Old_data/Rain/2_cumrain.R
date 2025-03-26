# Load necessary libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)


########################### daily data calculation

#################create daily output processed files with rain in mm

file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3"

# List all .csv files in the folder
csv_files <- list.files(file_path, pattern = "*.csv", full.names = TRUE)

# List all .csv files in the folder
files <-list.files(file_path, pattern = "*.csv", full.names = TRUE)
csv_files <-files[grepl("^output", basename(files), ignore.case = TRUE)]


for ( i in seq_along(csv_files)) {  
  csv_file <- csv_files[i]
  # Read the contents of the file
  file_contents <-  read.table(csv_file  , header = FALSE, sep = "\t") 
  # Add the file contents to the list
  
  colnames(file_contents)<-c("Date","tips")
  
  result_df_tips<- file_contents%>%
    mutate(date = mdy_hms(Date, tz = "America/Argentina/Buenos_Aires"))
  
  ##change NAs to 0
  result_df_tips[is.na(result_df_tips)] <- 0
  
  
  #prepare daily timeserie
  
  min_datetime <- Inf
  min_datetime  <- -Inf
  
  # Calculate the minimum and maximum date and time up to hour
  
  min_datetime_in_df <- min(result_df_tips$Date, na.rm = TRUE)
  max_datetime_in_df <- max(result_df_tips$Date, na.rm = TRUE)
  
  # Convert datetime values to POSIXct format
  
  min_datetime_in_df <- as.POSIXct(min_datetime_in_df, format = "%m/%d/%y %I:%M:%S %p")
  max_datetime_in_df <- as.POSIXct(max_datetime_in_df, format = "%m/%d/%y %I:%M:%S %p")
  
  #vector with hour interval
  time_step_day <- 1
  date_sequence <- seq(from = as.POSIXct( min_datetime_in_df ), to = as.POSIXct(max_datetime_in_df), by = paste(time_step_day, "days")) 
  daily_date_sequence <- format(date_sequence, "%Y/%d/%m")
  
  #create a vector with every hour between the min and max timestep
  daily_date_sequence<-as.data.frame(daily_date_sequence)
  colnames(daily_date_sequence)<-c("Date")
  
  
  result_df_daily<-result_df_tips%>%
    mutate(year= year(date))%>%
    mutate(month = month(date))%>%
    mutate(day = day(date))%>%
    #  mutate(hour = hour(Date))%>%
    #  mutate(min = minute(Date))%>%
    #  mutate(sec=second(Date))
    select(date,year,month,day,tips)%>%
    group_by(year,month,day)%>%
    # Use cumsum to summarize by jDay
    summarize(
      Dailymm = sum(tips))%>% 
    ##daily total tips
    ungroup()
  # Determine the output conversion factor based on the file name
  
  output_conversion_factor <- ifelse(grepl("EM",  csv_file ), 0.1, 0.254)
  browser() 
  # Convert tips to mm individually and write the table with daily tag
  Tipsmm <- result_df_daily %>%
    mutate(Date = make_date(year, month, day)) %>%
    mutate(Rainmm = Dailymm * output_conversion_factor) %>%
    mutate(date = format(Date, "%Y/%d/%m"))%>%
    select(date, Rainmm)
    
  
  browser()
  
  # Left join to match dates and add the Difference column
  daily_date_sequence1 <- daily_date_sequence %>%
    left_join(Tipsmm, by = c("Date" = "date"))
  
  # if unmatched dates, replace NAs with 0 
  daily_date_sequence1$Rainmm[is.na(daily_date_sequence1$Rainmm)] <- 0
  
  ######## daily file to write 
  
  # Save the processed data to a file
  output_file <- paste0("daily_", basename(csv_file))
  output_path <- paste0(file_path,"/","daily_", basename(csv_file))
  browser()
  write.table(daily_date_sequence1,  output_path , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  browser()
  cat("Processed file:", csv_file , "Saved output to:", output_file, "\n")
} 

####################create hourly files

file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3"

# List all .csv files in the folder
csv_files <- list.files(file_path, pattern = "*.csv", full.names = TRUE)

# List all .csv files in the folder
files <-list.files(file_path, pattern = "*.csv", full.names = TRUE)
csv_files <-files[grepl("^output", basename(files), ignore.case = TRUE)]

for ( i in seq_along(csv_files)) {  
  csv_file <- csv_files[i]
  # Read the contents of the file
  file_contents <-  read.table(csv_file  , header = FALSE, sep = "\t") 
  # Add the file contents to the list
 
colnames(file_contents)<-c("Date","tips")

result_df_tips<- file_contents%>%
  mutate(date = mdy_hms(Date, tz = "America/Argentina/Buenos_Aires"))


##change NAs to 0
result_df_tips[is.na(result_df_tips)] <- 0

min_datetime <- Inf
min_datetime  <- -Inf

# Calculate the minimum and maximum date and time up to hour

min_datetime_in_df <- min(result_df_tips$Date, na.rm = TRUE)
max_datetime_in_df <- max(result_df_tips$Date, na.rm = TRUE)

# Convert datetime values to POSIXct format

min_datetime_in_df <- as.POSIXct(min_datetime_in_df, format = "%m/%d/%y %I:%M:%S %p")
max_datetime_in_df <- as.POSIXct(max_datetime_in_df, format = "%m/%d/%y %I:%M:%S %p")

#vector with hour interval
time_step_hours <- 1
date_sequence <- seq(from = as.POSIXct(min_datetime_in_df ), to = as.POSIXct(max_datetime_in_df), by = paste(time_step_hours, "hours")) 

#create a vector with every hour between the min and max timestep
date_sequence<-as.data.frame(date_sequence)
colnames(date_sequence)<-c("Date")

hour_date_sequence <- date_sequence%>%
  mutate(datehour = format(Date, format = "%Y/%d/%m %H"))%>%
  select(datehour)

#prepare hourly dataset rounding down hours 

result_df_hourly <- result_df_tips %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")) %>%
  mutate(datehour = as.POSIXct(Date, format = "%Y/%d/%m %H", tz = "UTC")) %>%
  mutate(year = year(datehour)) %>%
  mutate(month = month(datehour)) %>%
  mutate(day = day(datehour)) %>%
  mutate(h = hour(datehour)) %>%
  select(year, month, day, h, tips) %>%
  group_by(year, month, day, h) %>%
  summarize(
    hourly = sum(tips)
  ) %>%
  ungroup()
  
  ####conversion factor from tips to mm
  
output_conversion_factor <- ifelse(grepl("EM",  csv_file ), 0.1, 0.254)
browser() 

# Convert tips to mm individually and write the table with hourly tag
Tipsmm <- result_df_hourly %>%
  mutate(Date = make_datetime(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day),
    hour = as.integer(h))) %>%
  mutate(Rainmm = hourly * output_conversion_factor) %>%
  mutate(date = format(Date, format = "%Y/%d/%m %H"))%>%
  select(date, Rainmm)


# Left join to match dates 

hour_date_sequence1 <- hour_date_sequence %>%
  left_join(Tipsmm, by = c("datehour" = "date"))

# if unmatched dates, replace NAs with 0 
hour_date_sequence1$Rainmm[is.na(hour_date_sequence1$Rainmm)] <- 0

######## hourly file to write 

# Save the processed data to a file
output_file <- paste0("hourly_", basename(csv_file))
output_path <- paste0(file_path,"/","hourly_", basename(csv_file))

browser()
write.table(hour_date_sequence1,  output_path , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
browser()
cat("Processed file:", file_path , "Saved output to:", output_file, "\n")
} 


#################create 2 minute files

file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3"

# List all .csv files in the folder
csv_files <- list.files(file_path, pattern = "*.csv", full.names = TRUE)

# List all .csv files in the folder
files <-list.files(file_path, pattern = "*.csv", full.names = TRUE)
csv_files <-files[grepl("^output", basename(files), ignore.case = TRUE)]


for ( i in seq_along(csv_files)) {  
  csv_file <- csv_files[i]
  # Read the contents of the file
  file_contents <-  read.table(csv_file  , header = FALSE, sep = "\t") 
  # Add the file contents to the list
  
  colnames(file_contents)<-c("Date","tips")
  
  result_df_tips<- file_contents%>%
    mutate(date = mdy_hms(Date, tz = "America/Argentina/Buenos_Aires"))
  
  ##change NAs to 0
  result_df_tips[is.na(result_df_tips)] <- 0
  
  
  min_datetime <- Inf
  min_datetime  <- -Inf
  
  # Calculate the minimum and maximum date and time up to hour
  
  min_datetime_in_df <- min(result_df_tips$Date, na.rm = TRUE)
  max_datetime_in_df <- max(result_df_tips$Date, na.rm = TRUE)
  
  # Convert datetime values to POSIXct format
  
  min_datetime_in_df <- as.POSIXct(min_datetime_in_df, format = "%m/%d/%y %I:%M:%S %p")
  max_datetime_in_df <- as.POSIXct(max_datetime_in_df, format = "%m/%d/%y %I:%M:%S %p")
  
  #vector with hour interval
  time_step_minutes <- 2
  date_sequence <- seq(from = as.POSIXct(min_datetime_in_df ), to = as.POSIXct(max_datetime_in_df), by = paste(time_step_minutes, "min")) 
  
  #create a vector with every hour between the min and max timestep
  date_sequence<-as.data.frame(date_sequence)
  colnames(date_sequence)<-c("Date")
  
  minute_date_sequence <- date_sequence%>%
    mutate(datemin = format(Date, format = "%Y/%d/%m %H%M"))%>%
    select(datemin)
  
  result_df_min<-result_df_tips%>%
    mutate(year= year(date))%>%
    mutate(month = month(date))%>%
    mutate(day = day(date))%>%
    mutate(hour = hour(date))%>%
    mutate(min = minute(date))%>%
    #  mutate(sec=second(Date))
    select(date,year,month,day,hour,min,tips)%>%
    group_by(year, month, day, hour, min) %>%  # group by year, month, day, hour, and 2-minute interval!!!
    # Use cumsum to summarize by jDay
    summarize(
      tips= sum(tips))%>% 
    ##daily total tips
    ungroup()
  
    #  write the table with minute tag
  Tipsmm <- result_df_min %>%
    mutate(Date = make_datetime(year, month, day, hour, min))

    ####### aggregate by two minutes
  
  interval_minutes <- 2
  
  result_df_2min <- Tipsmm %>%
    mutate(minute_interval = (minute(Date)+interval_minutes) %/% interval_minutes * interval_minutes) %>%
    group_by(year, month, day, hour, minute_interval) %>%
    summarise(tips2min = sum(tips)) %>%
    mutate(Date = make_datetime(year, month, day, hour, minute_interval))
  
    # Determine the output conversion factor based on the file name
  
  output_conversion_factor <- ifelse(grepl("EM",  csv_file ), 0.1, 0.254)
  browser() 
  # Convert tips to mm individually and write the table with daily tag
  Tipsmm <- result_df_2min %>%
    mutate(Rainmm = tips2min* output_conversion_factor) %>%
    select(Date, Rainmm)%>%
    mutate(date= format(Date, format = "%Y/%d/%m %H%M"))%>%
    ungroup()%>%
    select(date,Rainmm)
  
    # Left join to match dates 
  
 minute_date_sequence1 <- minute_date_sequence %>%
    left_join(Tipsmm, by = c("datemin" = "date"))
  
  # if unmatched dates, replace NAs with 0 
 minute_date_sequence1$Rainmm[is.na( minute_date_sequence1$Rainmm)] <- 0
  
  ######## 2 minute file to write 
  
    # Save the processed data to a file
  output_file <- paste0("2minute_", basename(csv_file))
  output_path <- paste0(file_path,"/","2minute_", basename(csv_file))
  
  browser()
  write.table( minute_date_sequence1,  output_path , sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
  browser()
  cat("Processed file:", file_path , "Saved output to:", output_file, "\n")
} 


##################################### END DAILY AND .TXT OUTPUT WITH INDIVIDUAL TIPS FOR 3 PLUVIOMETERS ###########################################

















