# process rain data
# 14/11/2023 NOT YET TESTED

# read user input
read_user_input <- function() {
  # Create empty lists to store user-provided file names and paths
  user_file_names <- list()
  user_file_paths  <- list() #
  
  # Prompt the user for file names until they press Enter
  while (TRUE) {
    # Prompt the user for a file name
    file <- readline("Enter a file name (or press Enter to finish): ")
    # Check if the user pressed Enter (i.e., entered an empty string)
    if (file == "") {
      break  # Exit the loop if the user pressed Enter
    }
    
    # Add the user-provided file name to the user_file_names list
    user_file_names <- append(user_file_names, basename(file))
    # Add the corresponding file path to the user_file_paths list
    # use dirname
    user_file_paths <- append(user_file_paths, dirname(file))
  }  
    return(list(names = user_file_names, paths = user_file_paths))
}

# read data file function
read_data_file <- function(name, select_col = 2:4) {
  #read in file
  data <- read_csv(name, header = FALSE, skip=2) %>%
    select(select_col)

  # Rename columns
  colnames(data) <- c("Date_Time", "Temp", "Event")

  # Convert Date_Time to POSIXct format
  data <- data %>%
    mutate(Date = mdy_hms(Date_Time, tz = "America/Argentina/Buenos_Aires"))%>%
    # Sort the data by 'Event' column and then by 'Date' (renamed Date_time column)
    # do we want to keep Date or Date_Time?
    arrange(Event, Date)%>%
    # Select 'Date' and 'Event' columns and remove rows with NAs (no data)
    select(Date, Event) %>%
    na.omit(data)
  
  return(data)
}

# write otip file
write_otip_file <- function(name, df) {
  # Define the output file path and name tag
  file_path <- input$user_file_paths[[which(input$user_file_name==name)]]
  #output_path <- paste0(file_path,"/","OTIP_", name)
  output_file <- paste0("OTIP_", name)
  #  browser()
  # Save the processed data to a csv file
  write_csv(df, file = paste0(file_path,"/",output_file), quote = none)
  return(list(path = file_path, file = output_file))
  
}


# read otip file
read_otip_file <- function(name){
  data_out <- read_csv(name)
  # make sure dates are working
  data_out <- data_out %>%
    mutate(date = mdy_hms(Date, tz = "America/Argentina/Buenos_Aires"))
  return(data_out)
}

# difference data
diff_data <- function(data) {
  data1 <- data%>%
    mutate(Difference = diff(Event)) %>%
           #ifelse(Event >= lag(Event, default = first(Event)), Event - lag(Event, default = first(Event)), 0))%>%
    select(Date, Difference)
  return(data1)
}


  
process_rain_data <- function(file_path){
  
  # step 1
  input <- read_user_input()
  # input is a list
  # # Create empty lists to store user-provided file names and paths
  # user_file_names <- list()
  # user_file_paths  <- list()
  # 
  # # Prompt the user for file names until they press Enter
  # while (TRUE) {
  #   # Prompt the user for a file name
  #   file <- readline("Enter a file name (or press Enter to finish): ")
  #   # Check if the user pressed Enter (i.e., entered an empty string)
  #   if (file == "") {
  #     break  # Exit the loop if the user pressed Enter
  #   }
  #   
  #   # Add the user-provided file name to the user_file_names list
  #   user_file_names <- append(user_file_names, list(file))
  #   # Add the corresponding file path to the user_file_paths list
  #   user_file_paths <- append(user_file_paths, list(file))
    
    # ###CHECK LIST
    # user_file_names
    # user_file_paths
    
    # Loop through each .csv file
    for (name in input$user_file_names) {
      
      data_process <- read_data_file(name)
      # # Read the .csv file
      # data <- read.csv(name, header = FALSE, sep = ",",skip=2)%>%
      #   select(2:4,)
      # 
      # # Rename columns
      # colnames(data) <- c("Date_Time", "Temp", "Event")
      # 
      # # Convert Date_Time to POSIXct format
      # data <- data %>%
      #   mutate(Date = mdy_hms(Date_Time, tz = "America/Argentina/Buenos_Aires"))%>%
      #   # Sort the data by 'Event' column and then by 'Date_Time'
      #   arrange(Event, Date_Time)%>%
      #   # Select 'Date_Time' and 'Event' columns and remove rows with NAs (no data)
      #   select(Date_Time, Event) %>%
      #   na.omit(data)
      #  browser()
      # data1 <-data%>%
      #   mutate(Difference = ifelse(Event >= lag(Event, default = first(Event)), Event - lag(Event, default = first(Event)), 0))%>%
      #   select(Date_Time, Difference)
      #  browser()
      # difference the events column
      data_process1 <- diff_data(data_process)
      
      # write to otip file
      out <- write_otip_file(name, data_process1)
      cat("Processed file:", out$file, "Saved output to:", out$path, "\n")
    }
}
#this fucntion now works only with Date Time format to outpit OTIP tag files 
#this files contain the individual tips


#################SECOND FUNCTION#############################
#######################NOW QUICK CHECK PLOT###############################
####### read the OIT files and calculate rain in mm, do a quick check plot

quick_check_plot <- function(file_path){
  #write enpty list to storage the files 
  data2_list<-list()
  # Specify the file path
  #file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3"
  setwd(file_path) # needed?
  
  # read user input
  input <- read_user_input()
  # # Create empty lists to store user-provided file names and paths
  # user_file_names <- list()
  # user_file_paths <- list()
  # 
  # # Prompt the user for file names until they press Enter
  # while (TRUE) {
  #   # Prompt the user for a file name
  #   file <- readline("Enter a file name (or press Enter to finish): ")
  #   
  #   # Check if the user pressed Enter (i.e., entered an empty string)
  #   if (file == "") {
  #     break  # Exit the loop if the user pressed Enter
  #   }
  #   # Add the user-provided file name to the user_file_names list
  #   user_file_names <- append(user_file_names, list(file))
  #   
  #   # Add the corresponding file path to the user_file_paths list
  #   user_file_paths <- append(user_file_paths, list(file.path(file_path, file)))
    
  # Loop through each .csv file
  for (name in user_file_names) {
      # Read the .csv file
      data <- read_otip_file(name)
      #options(warn = -1)
      # Specify the column positions to read in OTIP files 
      #col_positions <- fwf_widths(c(20, Inf))   ##OTIP file format
      # Read the file
      #data <- suppressMessages(read_fwf(user_file_paths, col_positions = col_positions))
      #Rename columns
      #colnames(data) <- c("Date", "Event") 
      
      # data1<-data%>%
      #   mutate(date = mdy_hms(Date, tz = "America/Argentina/Buenos_Aires"))
      # #  browser()
      # Check if "R1" or "R7" is in the file name at specific positions (e.g., at the beginning)
      #file_name <- basename(user_file_paths)
      if (grepl("R1|R7", name)){
        # Apply transformations based on file name
        data2 <- data %>%
          mutate(tipRainmm = Event * 0.254)%>%
          mutate(CumRainmm = cumsum(tipRainmm))%>%
          select(date,CumRainmm) # If "R1" or "R7" is in the file name
        
      } else {
        # Default transformation
        data2 <- data %>%
          mutate(tipRainmm = Event * 0.1)%>%
          mutate(CumRainmm = cumsum(tipRainmm))%>%
          select(date,CumRainmm)# Default transformation for other cases
        
      }
      
      # add the user_file_name to the list
      data2 <- data2 %>%
        mutate(filename = name)
      # Add data2 to the list
      data2_list[[length(data2_list) + 1]] <- data2
      cat("Processed file:", file_path, "\n")
    
    return(data2_list)
  }
}

## Quick check with manual data
quick_check_manual <- function(data_list) {
  
  # collapse the list
  result_df <- bind_rows(data_list)
  
  # #merge dataset of three pluviometers
  # # dates times across all data frames in data2_list. 
  # # Extract and stack the "Date" column from each data frame in data2_list
  # 
  # result_df <- bind_rows(data_list) %>%
  #   select(date) %>%
  #   arrange(date) %>%
  #   summarise(dates = unique(date))
  # 
  # # Loop through each element in data2_list
  # for (i in 1:length(data_list)) {
  #   # Left join the current data frame with the result data frame using JulianDate
  #   # Get the corresponding file name
  #   result_df <- left_join(result_df, data_list[[i]], by = c("date"), keep=FALSE)
  # }
  # 
  # #create a evctor with colnames
  # col_names<-c()
  # for (i in 1:length(user_file_names)) {
  #   # Get the current column name
  #   
  #   file_name <- user_file_names[i]
  #   
  #   # Append the file name to the corresponding column name
  #   col_names[i] <- paste0(file_name,col_names[i])
  # }
  # col_names_final<-c("Date",col_names)
  # 
  # colnames(result_df)<-col_names_final
  
  # result_df_mm <-result_df %>%
  #   mutate(date = mdy_hms("Date", tz = "America/Argentina/Buenos_Aires"))
  
  
  # Create a title with the download data date
  file_date <- substr(result_df$filename[1], 3, nchar(file_name))
  ggtitle_text <- paste("Rain Data Quick Check (File Number:", file_date, ")")
  
  
  # Reshape the data into long format
  #result_df_long <- pivot_longer(result_df, cols = -Date, names_to = "Variable", values_to = "Value")
  
  # Create the first scatter plot
  quickplot_rain <- ggplot(result_df, aes(x = Date, y = CumRainmm, color = filename)) +
    geom_point(size = 2) +
    # geom_line(size = 0.5)+
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text[1]) +
    theme_minimal() +
    theme_light() +  
    theme(legend.position = "top")
  
  p1<-ggplotly(quickplot_rain)
  
  # Create the plot
  quickplot_rain2 <- ggplot(result_df, aes(x = Date, y = CumRainmm, color = filename, group = filename)) +
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
  
}
