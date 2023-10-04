# This script loads one .csv file of cumulative tips rain data and converts to .csv divide by tabs (opened in notepad) with Date Time
# and rain in tips

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(readxl)
#library(orca)

# Specify the file path
file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3"
setwd(file_path)


#######  FIRST FUNCTION RUN
#file_path is the directory of Process file

#First function to run
process_flume_data (file_path = "C:/Users/Lenovo/Downloads/SampleFiles3")
#EM061523.csv
#Second function to run
quick_check_plot (file_path = "C:/Users/Lenovo/Downloads/SampleFiles3")
#OTIP_EM061523.csv
#precipitacionescorona2023

#################FRIST FUNCTION#############################

process_flume_data <- function(file_path){
# Create empty lists to store user-provided file names and paths
user_file_names <- list()
user_file_paths  <- list()
user_file_data <-list()
# Prompt the user for file names until they press Enter
while (TRUE) {
  # Prompt the user for a file name
  file <- readline("Enter a file name (or press Enter to finish): ")
    # Check if the user pressed Enter (i.e., entered an empty string)
  if (file == "") {
    break  # Exit the loop if the user pressed Enter
  }
  
  # Add the user-provided file name to the user_file_names list
  user_file_names <- append(user_file_names, list(file))
    # Add the corresponding file path to the user_file_paths list
  user_file_paths <- append(user_file_paths, list(file))
  # Extract the file prefix first two letters
  file_prefix <- substr(file, 1, 2)
  file_date <- substr(file, 3, 8)
  # Add the file name and prefix to the list
  user_file_data <- append(user_file_data, list(list(file_name = file ,prefix = file_prefix, date = file_date)))
  }

# Return the list of file data
return(user_file_data)
}
##########Analyse the instrument to analyse .csv
# matches the target prefix
  if (file_prefix == target_prefix) {
    # Perform your processing and produce output here
    cat("Processing file:", file, "\n")
    # Example: read and print the contents of the file
    # data <- read.table(file, header = TRUE)
    # print(data)
  }
}

# Return any results or summaries you want
# For example, you can return a summary or a list of processed data
# return(processed_data)
}

# Call the function to process files starting with "S1"
results_S1 <- process_files(target_prefix = "S1")

  # --- HOBOU20 ---- S2111219.csv
  


# Loop through each .csv file
for (user_file_paths in user_file_names) {
  # Read the .csv file
  data <- read.csv(user_file_paths, header = FALSE, sep = ",",skip=2)%>%
    select(2:4,)
  
  # Rename columns
  colnames(data) <- c("Date_Time", "Temp", "Event")
  
  # Convert Date_Time to POSIXct format
  data <- data %>%
    mutate(Date = mdy_hms(Date_Time, tz = "America/Argentina/Buenos_Aires"))%>%
    # Sort the data by 'Event' column and then by 'Date_Time'
    arrange(Event, Date_Time)%>%
  # Select 'Date_Time' and 'Event' columns and remove rows with NAs (no data)
    select(Date_Time, Event) %>%
    na.omit(data)
#  browser()
  data1 <-data%>%
    mutate(Difference = ifelse(Event >= lag(Event, default = first(Event)), Event - lag(Event, default = first(Event)), 0))%>%
    select(Date_Time, Difference)
#  browser()
  # Define the output file path and name tag
  output_path <- paste0(user_file_paths,"/","OTIP_", basename(user_file_paths))
  output_file <- paste0("OTIP_", basename(user_file_paths))
#  browser()
  # Save the processed data to a file
  write.table(data1, file = output_file, sep = "\t", col.names = FALSE, row.names = FALSE,quote = FALSE)
  
  cat("Processed file:", basename(user_file_paths), "Saved output to:", output_file, "\n")
}
}
}
#this fucntion now works only with Date Time format to outpit OTIP tag files 
#this files contain the individual tips

###########################################################


#################SECOND FUNCTION#############################
#######################NOW QUICK CHECK PLOT###############################
####### read the OIT files and calculate rain in mm, do a quick check plot

quick_check_plot <- function(file_path){
  #write enpty list to storage the files 
  data2_list<-list()
  # Specify the file path
  file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3"
  setwd(file_path)
  
  # Create empty lists to store user-provided file names and paths
  user_file_names <- list()
  user_file_paths <- list()
  
  # Prompt the user for file names until they press Enter
  while (TRUE) {
    # Prompt the user for a file name
    file <- readline("Enter a file name (or press Enter to finish): ")
    
    # Check if the user pressed Enter (i.e., entered an empty string)
    if (file == "") {
      break  # Exit the loop if the user pressed Enter
    }
    # Add the user-provided file name to the user_file_names list
    user_file_names <- append(user_file_names, list(file))
    
    # Add the corresponding file path to the user_file_paths list
    user_file_paths <- append(user_file_paths, list(file.path(file_path, file)))
    
  # Loop through each .csv file
  for (user_file_paths in user_file_names) {
    # Read the .csv file
    options(warn = -1)
    # Specify the column positions to read in OTIP files 
    col_positions <- fwf_widths(c(20, Inf))   ##OTIP file format
    # Read the file
    data <- suppressMessages(read_fwf(user_file_paths, col_positions = col_positions))
    #Rename columns
    colnames(data) <- c("Date", "Event") 
    
    data1<-data%>%
      mutate(date = mdy_hms(Date, tz = "America/Argentina/Buenos_Aires"))
  #  browser()
    # Check if "R1" or "R7" is in the file name at specific positions (e.g., at the beginning)
    file_name <- basename(user_file_paths)
        if (grepl("R1|R7", file_name)){
        # Apply transformations based on file name
        data2 <- data1 %>%
          mutate(tipRainmm = Event * 0.254)%>%
          mutate(CumRainmm = cumsum(tipRainmm))%>%
          select(date,CumRainmm) # If "R1" or "R7" is in the file name
        
      } else {
        # Default transformation
        data2 <- data1 %>%
          mutate(tipRainmm = Event * 0.1)%>%
          mutate(CumRainmm = cumsum(tipRainmm))%>%
          select(date,CumRainmm)# Default transformation for other cases
        
  }
    }
  
  
    # Add data2 to the list
    data2_list[[length(data2_list) + 1]] <- data2
    
    cat("Processed file:", file_path, "\n")
  }
  ###################this now works with date and time
  # ###check list
  # head(data2_list[[1]]) 
  # head(data2_list[[2]]) 
  # head(data2_list[[3]]) 
  
  #################THIRD FUNCTION#############################
  
  #############NOW QUICK CHECK WITH MANUAL DATA
  
  #merge dataset of three pluviometers
  # dates times across all data frames in data2_list. 
  # Extract and stack the "Date" column from each data frame in data2_list
  
  stacked_df <- bind_rows(lapply(data2_list, function(df) select(df, date))) %>%
    arrange(date)
  
  result_df <-unique(stacked_df)
  
  # Loop through each element in data2_list
  for (i in 1:length(data2_list)) {
    # Left join the current data frame with the result data frame using JulianDate
    # Get the corresponding file name
    result_df <- left_join(result_df, data2_list[[i]], by = c("date"), keep=FALSE)
  }
  
  #create a evctor with colnames
  col_names<-c()
  for (i in 1:length(user_file_names)) {
    # Get the current column name
  
    file_name <- user_file_names[i]
    
    # Append the file name to the corresponding column name
    col_names[i] <- paste0(file_name,col_names[i])
  }
  col_names_final<-c("Date",col_names)
  
  colnames(result_df)<-col_names_final
  
  # result_df_mm <-result_df %>%
  #   mutate(date = mdy_hms("Date", tz = "America/Argentina/Buenos_Aires"))
  
  
  # Create a title with the download data date
  file_date <- substr(user_file_names, 3, nchar(file_name))
  ggtitle_text <- paste("Rain Data Quick Check (File Number:", file_date, ")")
  
  
  # Reshape the data into long format
  result_df_long <- pivot_longer(result_df, cols = -Date, names_to = "Variable", values_to = "Value")
  
  # Create the first scatter plot
  quickplot_rain <- ggplot(result_df_long, aes(x = Date, y = Value, color = Variable)) +
    geom_point(size = 2) +
  # geom_line(size = 0.5)+
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text[1]) +
    theme_minimal() +
    theme_light() +  
    theme(legend.position = "top")
  
  p1<-ggplotly(quickplot_rain)
  
  # Create the plot
  quickplot_rain2 <- ggplot(result_df_long, aes(x = Date, y = Value, color = Variable, group = Variable)) +
    geom_col(width = 0.5) +
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text[1]) +
    theme_minimal() +
    theme_light() +  # Use a minimal theme
    theme(legend.position = "top")
  
  # # Format the date axis with one-hour intervals and specify the date format
  # quickplot_rain2 <- quickplot_rain2 +
  #   scale_x_datetime(
  #     date_breaks = "1 day",              # Set the breaks to display at one-hour intervals
  #     date_labels = "%Y-%m-%d %H:%M:%S"  # Desired date format
  #   )
  
  # Convert the plot to a Plotly object
  p2 <- ggplotly(quickplot_rain2)

  
p3<-subplot(p1,p2)
print(p3)

ggsave(quickplot_rain, filename = "quickcheck1.png", width = 8, height = 6, dpi = 300)
ggsave(quickplot_rain2, filename = "quickcheck2.png",width = 8, height = 6, dpi = 300)
cat("Quick check plot displayed and saved")

# Initialize empty lists to store file names and paths
Muser_file_names <- list()
Muser_file_paths <- list()

# Prompt the user for file names until they press Enter
while (TRUE) {
  # Prompt the user for a file name
  Mfile <- readline("Enter the Excel file name (or press Enter to finish): ")
  
  # Check if the user pressed Enter (i.e., entered an empty string)
  if (Mfile == "") {
    break  # Exit the loop if the user pressed Enter
  }
  
  # Add the user-provided file name to the Muser_file_names list
  Muser_file_names <- append(Muser_file_names, list(Mfile))
  
  # Add the corresponding file path to the Muser_file_paths list
  Muser_file_paths <- append(Muser_file_paths, list(file.path(file_path, Mfile)))
 
  # Initialize an empty list to store data frames
  data_frames_list <- list()
  
  # Loop through each Excel file path
  for (Muser_file_path in Muser_file_paths) {
    # Read the Excel file
    options(warn = -1)
    
    data_frame <- read_excel(Muser_file_path, skip = 2)
    
    excel_sheets <- excel_sheets(Muser_file_path)
    
    # Create an empty list to store data frames for each sheet in the current file
    sheet_data_frames_list <- list()
    
    # Loop through each sheet in the current file (except the last one)
    for (sheet_name in excel_sheets[-length(excel_sheets)]) {
      # Read the sheet into a data frame
      
      sheet_data_frame1 <- read_excel(Muser_file_path,skip=0, sheet = sheet_name)
      
      num_columns <- ncol(sheet_data_frame1)
      
      # Define the column types for reading
      col_types <- c("date", rep("numeric", 9), rep("text", num_columns - 10))
      
      sheet_data_frame2 <- read_excel(Muser_file_path,skip=2,sheet = sheet_name,col_types = col_types)
      
      # # Find the last row where "Total" is found in the first column
      # last_row <- nrow(sheet_data_frame2)
      # while (last_row > 0 && sheet_data_frame2[[1]][last_row] != "Total") {
      #   last_row <- last_row - 1
      # }
      # 
      # # Subset the data frame up to the last row
      # sheet_data_frame2 <-  sheet_data_frame2 [1:last_row,]
      
      # Append the data frame for this sheet to the list
      sheet_data_frames_list <- append(sheet_data_frames_list, list(sheet_data_frame2))
    }
    # Combine all data frames for the current file (except the last one) into one dataframe
    combined_data_frame <- bind_rows(sheet_data_frames_list)
  }
  
  ## sacar a R4 y ponerle hora 07:00 am 
  ## a los demas, ponerle la primer hora de la planilla y tambien al promedio
  combined_data_frame 
  
  combined_data_frame_av <- combined_data_frame[1:10] %>%
    rowwise() %>%
    mutate(Average_Rain = if (sum(!is.na(c_across(-Fecha))) > 1) {
      mean(c_across(-Fecha), na.rm = TRUE)
    } else {
      NA_real_
    }) %>%
    ungroup() %>%
    filter(!is.na(Fecha))%>%
    mutate(date = ymd(as.Date(Fecha)))%>%
    select(-Fecha)
  
  result_df_plotmanual<- result_df %>%
  mutate(date = ymd=(as.Date(Date)))%>%
    select(-Date)
  
  combined_data_frame_plot<-left_join(result_df_plotmanual, combined_data_frame_av, by=c("date"),everythig=TRUE)
    
        # Create a title with the download data date
    file_date <- substr(user_file_names, 3, nchar(file_name))
  ggtitle_text <- paste("Rain Data Quick Check (File Number:",colnames(result_df[2]),"and manual)")
  
  
  # Reshape the data into long format
  result_df_long_2 <- pivot_longer(combined_data_frame_plot, cols = -date, names_to = "Variable", values_to = "Value")
  
  # Create the first scatter plot
  quickplot_rain_manual <- ggplot(result_df_long_2, aes(x = date, y = Value, color = Variable)) +
    geom_point(size = 2) +
    # geom_line(size = 0.5)+
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text[1]) +
    theme_minimal() +
    theme_light() +  
    theme(legend.position = "top")
  pM<-ggplotly( quickplot_rain_manual)
  print(pM)
  
  ggsave(quickplot_rain_manual, filename = "quickcheck3.png", width = 8, height = 6, dpi = 300)
  # Save the processed data to a file
  write.table(combined_data_frame_plot, file = "manual", sep = "\t", col.names = FALSE, row.names = FALSE,quote = FALSE)
  
  cat("Quick check plot with manual data for the period displayed and saved")
  
}  


}



#############If all looks OK calculate daily rainfall
#######################If not modify the .csv file and run the code again
############################calculate daily rainfall


