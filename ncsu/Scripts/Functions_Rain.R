# process rain data
# 14/11/2023 NOT YET TESTED
# 03/12/2023 All tested and working, see TestRainfall.qmd

# read user input
read_user_input <- function(in_path) {
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
    # use in_path from input if dirname not specified in the input
    if (dirname(file) == ".") {
      user_file_paths <- append(user_file_paths, in_path)
    } else user_file_paths <- append(user_file_paths, dirname(file))
  }  
 # browser()
    return(list(names = list_c(user_file_names), paths = list_c(user_file_paths)))
}

# read automatically list of files
read_list_files <- function(file_path, file_ext = ".csv") {
  # this works if all files are in the same directory
  #browser()
  user_file_list <- dir(file_path, pattern = file_ext)
  user_path <- file_path
  # return same structure as read_user_input
  return(list(names = user_file_list, 
              paths = rep(user_path, length(user_file_list))))
}

# read data file function
read_data_file <- function(name, file_path, select_col = 2:4) {
  #read in file
  #browser()
  data <- read_csv(paste(file_path,name,sep="/"), col_names = FALSE, skip=2) %>%
    select(all_of(select_col))

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
write_otip_file <- function(name, df, input = input, output_path) {
  # Define the output file path and name tag
  #browser()
  file_path <- input$paths[[which(input$names==name)]]
  #output_path <- paste0(file_path,"/","OTIP_", name)
  output_file <- paste0("OTIP_", name)
    #browser()
  # Save the processed data to a csv file
  write_csv(df, file = paste0(file_path,"/", output_path, "/", output_file), quote = "none")
  return(list(path = file_path, file = output_file))
  
}


# read otip file
read_otip_file <- function(name, path_to_file){
  data_out <- read_csv(paste0(path_to_file,"/",name))
  # make sure dates are working
  data_out <- data_out %>%
    mutate(date = ymd_hms(Date, tz = "America/Argentina/Buenos_Aires")) %>%
    # rename Difference to Event
    rename("Event" = "Difference")
  return(data_out)
}

# difference data
diff_data <- function(data) {
  data1 <- data%>%
    mutate(Difference = c(0,diff(Event))) %>%
           #ifelse(Event >= lag(Event, default = first(Event)), Event - lag(Event, default = first(Event)), 0))%>%
    select(Date, Difference)
  return(data1)
}

##############
###############################################
###############################################
#'first plotting function
#'
#'@param df a data frame/tibble as input
#'@param ggtitle_text character, the text for the plot title
#'
#'@return a plotly object
#'
#'@examples 
#'NA
#'
#'@export

first_quick_plot <- function(df, ggtitle_text) {
# Create the first scatter plot
  quickplot_rain <- ggplot(df, aes(x = date, y = CumRainmm, color = filename)) +
    geom_point(size = 2) +
  # geom_line(size = 0.5)+
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text) +
    theme_minimal() +
    theme_light() +  
    theme(legend.position = "top")

  return(p1<-ggplotly(quickplot_rain))
}

###############################################
###############################################
#'second plotting function
#'
#'@param df a data frame/tibble as input
#'@param ggtitle_text character, the text for the plot title
#'
#'@return a plotly object
#'
#'@examples 
#'NA
#'
#'@export
second_quick_plot  <- function(df, ggtitle_text) {
  # Create the plot
  quickplot_rain2 <- ggplot(df, aes(x = date, y = CumRainmm, color = filename, group = filename)) +
    geom_col(width = 0.5) +
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text) +
    theme_minimal() +
    theme_light() +  # Use a minimal theme
    theme(legend.position = "top")

  # Convert the plot to a Plotly object 
  return(p2 <- ggplotly(quickplot_rain2))

}

###############################################
#'third plotting function
#'
#'@param df a data frame/tibble as input
#'
#'@return a plotly object
#'
#'@examples 
#'NA
#'
#'@export

third_quick_plot <- function(df) {
  
  # Create the plot
  quickplot_rain3 <- ggplot(na.omit(df), aes(x = CumRainmm.x, y = CumRainmm.y)) +
    geom_point(size=0.5) +
    theme_minimal() +
    theme_light() +
    theme(legend.position = "top")#+
    #abline(h = 0, v = 0, col = "red", lty = 2) +  # Add abline
  

  # Convert the plot to a Plotly object 
  p3 <- ggplotly(quickplot_rain3)
  
  p3 <- p3 %>%
    layout(
      xaxis = list(title = unique(na.omit(df)$filename.x)),
      yaxis = list(title = unique(na.omit(df)$filename.y))
    )
  
  # Add abline with add_trace to plotly object
  p3 <- p3 %>%
    add_trace(
      type = "scatter",
      mode = "lines",
      x = c(min(na.omit(df)$CumRainmm.x), max(na.omit(df)$CumRainmm.x)),
      y = c(min(na.omit(df)$CumRainmm.x), max(na.omit(df)$CumRainmm.x)),
      line = list(color = "blue", width = 1),
      hoverinfo = "1:1"
    )
  
  return(p3)
}

  
process_rain_data <- function(file_path, output_path_add = "processed"){
  
  # step 1
  input <- read_user_input(file_path)
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
  
    #browser()
    # Loop through each .csv file
    for (name in input$names) {
      data_process <- read_data_file(name, file_path)
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
      out <- write_otip_file(name, data_process1, input = input, output_path_add)
      cat("Processed file:", out$file, "Saved output to:", paste(out$path, output_path_add, sep = "/" ), "\n")
    }
}
#this fucntion now works only with Date Time format to outpit OTIP tag files 
#this files contain the individual tips

# second version of the above function that automatically runs through all files
process_rain_data_auto <- function(file_path, output_path_add = "processed"){
  #browser()
  # step 1
  input <- read_list_files(file_path)
  # input is a list
  #browser()
  # Loop through each .csv file
  for (name in input$names) {
    
    data_process <- read_data_file(name, file_path)
    # # Read the .csv file
    # difference the events column
    data_process1 <- diff_data(data_process)
    
    # write to otip file
    out <- write_otip_file(name, data_process1, input = input, output_path_add)
    cat("Processed file:", out$file, "Saved output to:", paste(out$path, output_path_add,sep="/"), "\n")
  }
}



#################SECOND FUNCTION#############################
#######################NOW QUICK CHECK PLOT###############################
####### read the OIT files and calculate rain in mm, do a quick check plot

create_data_list <- function(path_to_processed, file_numbers = "all"){
  #write empty list to store the files 
  data2_list<-list()
  #browser()
  input <- read_list_files(path_to_processed)
  # input is a list
  
  # if file_numbers is a number or series of numbers
  if (file_numbers != "all") {
    input_names <- input$names[file_numbers]
  } else input_names <-  input$names
  # Loop through each .csv file
  for (name in input_names) {
      # Read the .csv file
      data <- read_otip_file(name, path_to_processed)
      # Check if "R1" or "R7" is in the file name at specific positions (e.g., at the beginning)
      #file_name <- basename(user_file_paths)
      if (grepl("R1|R7|R4", name)){
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
      #browser()
      # add the user_file_name to the list
      data2 <- data2 %>%
        mutate(filename = name)
      # Add data2 to the list
      data2_list[[name]] <- data2
      cat("Processed file:", path_to_processed, "\n")
  }
    return(data2_list)
}

## Plot the data
check_plot <- function(data_list, plot = 1) {
  
  # collapse the list
  result_df <- bind_rows(data_list)
  #browser()
  
  # Create a title with the download data date
  filename1 <- result_df$filename[1]
  file_date <- substr(filename1, 8, nchar(filename1))
  ggtitle_text <- paste("Rain Data Quick Check (File Number:", file_date, ")")
  
  if (plot == 1) { # default
    (plot1 <- first_quick_plot(result_df, ggtitle_text))
  } else (plot2 <- second_quick_plot(result_df, ggtitle_text))
  
  
  
}


# match first break point in R1, R4 or R7 To plot one pluviometer against the other
match_breaks <- function(df) {
  # Check if filename contains 'R1' or 'R7'
  if (any(grepl("R1|R7|R4", df$filename))) {
    # Find the first value in CumRainmm column different from 0 - first break
    first_nonzero_index <- which(df$CumRainmm != 0)[1]
    
    # Extract the time series from the first non-zero value onwards
    time_series <- df %>%
      slice(first_nonzero_index:n()) %>%
      mutate(order = row_number())%>%
      select(order,filename, date, CumRainmm)
    
    return(time_series)
  } else {
    return(NULL)  # for filenames not containing 'R4' 'R1' or 'R7'
  }
}


