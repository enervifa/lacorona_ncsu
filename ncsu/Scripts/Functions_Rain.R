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
    mutate(`Date and Time` = ymd_hms(Date, tz = "America/Argentina/Buenos_Aires")) %>%
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

###############################################
###############################################
#'first plotting function
#'
#'@param df a data frame/tibble as input
#'@param ggtitle_text character, the text for the plot title
#'
#'@return a ggplot object
#'
#'@examples 
#'NA
#'
#'@export

first_quick_plot <- function(df, ggtitle_text) {
# Create the first scatter plot
  quickplot_rain <- ggplot(df, aes(x = Date, y = CumRainmm, color = filename)) +
    geom_point(size = 2) +
  # geom_line(size = 0.5)+
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text) +
    theme_minimal() +
    theme_light() +  
    theme(legend.position = "top")

  return(quickplot_rain)
}

###############################################
###############################################
#'second plotting function
#'
#'@param df a data frame/tibble as input
#'@param ggtitle_text character, the text for the plot title
#'
#'@return a ggplot object
#'
#'@examples 
#'NA
#'
#'@export
second_quick_plot  <- function(df, ggtitle_text) {
  # Create the plot
  quickplot_rain2 <- ggplot(df, aes(x = Date, y = CumRainmm, color = filename, group = filename)) +
    geom_col(width = 0.5) +
    labs(x = "Date", y = "Cumulative Rain in mm") +
    ggtitle(ggtitle_text) +
    theme_minimal() +
    theme_light() +  # Use a minimal theme
    theme(legend.position = "top")

  # return plot object
  return(quickplot_rain2)

}


###############################################
#'third plotting function
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
third_quick_plot  <- function(df, ggtitle_text) {
  if(require(GGally) == F) {message("You need the GGally library for this plot")}
  # group by hour
  plot_df <- df %>%
    group_by(Date_hour = lubridate::floor_date(Date, "1 hour"),filename) %>%
    summarise(Hourly_Rainfall = sum(tipRainmm)) %>%
    pivot_wider(names_from =  filename, 
                values_from = Hourly_Rainfall) %>%
    mutate(across(contains("OTIP"),~ ifelse(is.na(.x),0,.x)))
  
  quickplot_rain3 <- ggpairs(plot_df[,2:4],
                             title = ggtitle_text,
                             upper = list(continuous = "cor"),
                             lower = list(continuous = "smooth")) +
    theme_minimal() +
    theme_light() +  # Use a minimal theme
    theme(legend.position = "top")
  
  # Convert the plot to a Plotly object 
  return(quickplot_rain3)
  
}



#####################################################################
#' process rain data main function
#' 
#' This is the main function to process the measured rainfall data
#' @param filepath
#' @param output_path_add text string for an additional path variable to move processed files
#' @example 
#' process_rain_data("../Rain")
#' 
#' @export
process_rain_data <- function(file_path, output_path_add = "processed"){
  
  # step 1
  input <- read_user_input(file_path)
  # input is a list

    #browser()
    # Loop through each .csv file
    for (name in input$names) {
      data_process <- read_data_file(name, file_path)
 
      # difference the events column
      data_process1 <- diff_data(data_process)
      
      # write to otip file
      out <- write_otip_file(name, data_process1, input = input, output_path_add)
      cat("Processed file:", out$file, "Saved output to:", paste(out$path, output_path_add, sep = "/" ), "\n")
    }
}
#this function now works only with Date Time format to output OTIP tag files 
#this files contain the individual tips

#####################################################################
#' second version of process rain data main function
#' 
#' second version of the above function that automatically runs through all files
#' @param filepath
#' @param output_path_add text string for an additional path variable to move processed files
#' @example 
#' process_rain_data_auto("../Rain")
#' 
#' @export
process_rain_data_auto <- function(file_path, output_path_add = "processed"){
  #browser()
  # check if folder 'processed' exists
  if (dir.exists(paste0(file_path,"/", output_path_add))==F) {
    dir.create(paste0(file_path,"/", output_path_add))
  }
  
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
    #browser()
    # write to otip file
    out <- write_otip_file(name, data_process1, input = input, output_path_add)
    cat("Processed file:", out$file, "Saved output to:", paste(out$path, output_path_add,sep="/"), "\n")
  }
}



#####################################################################
#' read the OTIP files and calculate rain in mm
#' 
#' This sets up the list for the plotting
#' @param path_to_processed path where the OTIP files are
#' @param file_numbers optional, whether you want to read just a few files, defaults to "all"
#' @example 
#' create_data_list("../Rain/processed")
#' 
#' @export
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
  
  # Loop through each .csv file to find last date
  start_dates <- list()
  #browser()
  for (name in input_names) {
      # Read the .csv file
      data <- read_otip_file(name, path_to_processed)
    
      #extract the first date from the file
      start_dates[name] <- as.Date(data$Date[1])
  }
  print(start_dates)
  # find the latest start date
  last_date <- max(unlist(start_dates))
  
  
  # Loop through each .csv file
  for (name in input_names) {
      # Read the .csv file
      data <- read_otip_file(name, path_to_processed)
      # Check if "R1" or "R7" is in the file name at specific positions (e.g., at the beginning)
      #file_name <- basename(user_file_paths)
     # browser()
      if (grepl("R1|R7", name)){
        # Apply transformations based on file name
        data2 <- data %>%
          mutate(tipRainmm = Event * 0.254)%>% # If "R1" or "R7" is in the file name
          filter(Date >= as.Date(last_date)) %>% # filter by latest date
          mutate(CumRainmm = cumsum(tipRainmm))%>%
          select(Date,CumRainmm, tipRainmm) 
        if (data$Date[1] < as.Date(last_date)) {
          # write warning
          warning(paste(last_date, "used as start date, some data exists before this date"))
        }
      } else {
        # Default transformation for EM file
        data2 <- data %>%
          mutate(tipRainmm = Event * 0.1)%>%
          filter(Date >= as.Date(last_date)) %>% # filter by latest date
          mutate(CumRainmm = cumsum(tipRainmm))%>%
          # select Date, cumulative rain and the rain in mm
          select(Date,CumRainmm, tipRainmm)# Default transformation for other cases
        if (data$Date[1] < as.Date(last_date)) {
          # write warning
          warning(paste(last_date, "used as start date, some data exists before this date"))
        }
        
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

#####################################################################
#' Create a check plot over time
#' 
#' This plots the cumulative rainfall over time
#' @param data_list the input data list of dfs with rainfall data
#' @param plot type of plot to produce, defaults to 1
#' @example 
#' check_plot(df_list)
#' 
#' @export
## Plot the data
check_plot <- function(data_list, plot = 1) {

    
  # collapse the list
  result_df <- bind_rows(data_list)
  #browser()

  # Create a title with the download data date
  filename1 <- result_df$filename[1]
  file_date <- substr(filename1, 8, nchar(filename1))
  
  if (plot == 1) {
      ggtitle_text <- paste("Rain Data Quick Check Date:", file_date)
      p <- first_quick_plot(result_df, ggtitle_text)
      } else {
        if (plot == 2) {
          ggtitle_text <- paste("Rain Data Quick Check Date:", file_date)
          p <- second_quick_plot(result_df, ggtitle_text)
        } else { #plot == 3
          ggtitle_text <- paste("Scatter plot for Date:" , file_date)
          p <- third_quick_plot(result_df, ggtitle_text)
        }
      }
  ggplotly(p)
}

