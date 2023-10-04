library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Specify the file and output directories
file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3"
read_dir<-"C:/Users/Lenovo/Downloads/SampleFiles3"
output_dir <- "C:/Users/Lenovo/Downloads/SampleFiles3/output_plots"

user_file_data<-list()

# Call the function to collect user input and process files
user_file_data <- process_flume_data(file_path)

# Create a list to store processed data for each prefix
processed_data_list <- list() #
# Iterate through the user_file_data list
for (file_info in user_file_data) {
  # Process and plot files based on conditions
  processed_data <- process_and_plot_files(file_info)
  # Store the processed data in the list
  key <- paste(file_info$file_name, file_info$wich_catch, file_info$ins, sep = "_")
  processed_data_list[[key]] <- processed_data
}

############FIRST FUNCTION PROCESS DATA####################

#If you are not in a folder with all files to be processed e.g Flumes/Hobo20
# You can use process_flume_data to define a function to collect user input for file names
#instruments Hobo20, StevensU12
process_flume_data <- function(file_path) {
  # Create empty lists to store user-provided file data
  user_file_data <- list()
  
  # Prompt the user for file names until they press Enter
  while (TRUE) {
    # Prompt the user for a file name
    catchment <- readline("Enter catchment name V1, V2, V3 or V4 (or press Enter to finish): ")
    instrument <- readline("Enter a instrument name (or press Enter to finish): ")
    file <- readline("Enter a file name (or press Enter to finish): ")
    # Check if the user pressed Enter (i.e., entered an empty string)
    if (instrument == "" ) {
      break  # Exit the loop if the user pressed Enter
    }
    
    # Construct the full file path
    full_file_path <- file.path(file_path, file)
    
    # # Extract the file prefix first two letters
    # file_prefix <- substr(file, 1, 2)
    file_date <- substr(file, 3, 8)
    
    # Add the file data to the user_file_data list
    user_file_data <- append(user_file_data, list(list(ins = instrument,
                                                       file_name = file,
                                                      # prefix = file_prefix,
                                                       date = file_date,
                                                       full_path = full_file_path,
                                                       which_catch = catchment)))
  }
  
  # Return the list of file data
  return(user_file_data)
}

################ SECOND function to process files and save individual plots############
process_and_plot_files <- function(file_info) {
  filename <- file_info$file_name
  file_path <- file_info$full_path
  
  # Perform data transformations and analysis based on conditions
  file_ins <- file_info$ins
  
  if (file_ins %in% c("Hobo20")) {
  # Process and plot files from HOBO20 in V1V2, and "v3p"," V4p" from V3V4
    browser()
  read_hobou20(filename = filename, input_dir = read_dir ,plotit = T) 
  return(result)
  
 # } else if (file_ins %in% c("StevensU12"))  {
 browser()
 read_stevens(filename, input_dir = read_dir, plotit = T)
 read_stevens(filename, input_dir = read_dir, plotit = T)
 #    
 # } else {

  }
  # # Save individual plot as PNG
  # plot_filename <- file.path(output_dir, paste0(filename, "_plot.png"))
  # ggsave(filename = plot_filename, plot = p, device = "png")
  
}


# # Define a function to create and save combined plots
# create_combined_plot <- function(file_data, prefix, output_dir) {
#   # Combine data frames with the same prefix
#   combined_data <- do.call(rbind, file_data)
#   
#   # Create a combined plot
#   p <- ggplot(combined_data, aes(x = Date, y = Value)) +
#     geom_line() +
#     labs(title = paste("Combined Plot for Prefix:", prefix))
#   
#   # Save the combined plot as PNG
#   plot_filename <- file.path(output_dir, paste0(prefix, "_combined_plot.png"))
#   ggsave(filename = plot_filename, plot = p, device = "png")
# }
# 
# 
# # Create the output directory if it doesn't exist
# dir.create(output_dir, showWarnings = FALSE)



# --- HOBOU20 ----

read_hobou20 <- function(filename, input_dir ,
                         coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                         skip = 1, plotit = F) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  file_read <- file_read %>%
    mutate(`Date and Time` = force_tz(mdy_hms(`Date Time, GMT-03:00`),
                                      tz = "America/Argentina/Buenos_Aires")) 
  colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, ?C",
                                "Bar Pressure kPa",
                                "Water Level, meters")
  ggtitle_text <- paste("Rain Data Quick Check (File Number:", filename,")")
  
  file_out <- file_read %>%
    select(`Date and Time`, `Temp, ?C`,
           `Water Level, meters`)
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Temp, ?C`:`Water Level, meters`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")+
      ggtitle(ggtitle_text)
    print(p)
    
    # Save individual plot as PNG
    plot_filename <- file.path(output_dir, paste0(filename, "_plot.png"))
    ggsave(filename = plot_filename, plot = p, device = "png",width = 10, height = 8, dpi = 300)
  }
  return(file_out %>% select(`Date and Time`,
                             `Water Level, meters`))
  
}

# --- HOBOU12--- Stevens

# read the stevens logger
read_stevens <- function(filename, input_dir,
                         skip = 1, plotit = F) {
  #browser()
  # messy, but currently the only way I can do this
  if(grepl("V1",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c","c","c")
  if(grepl("V2",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c")
  if(grepl("V3",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","c","c","c","c","c")
  if(grepl("V4",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","c","c","c")
  
  browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  file_read <- file_read %>%
    mutate(`Date and Time` = force_tz(mdy_hms(`Date Time, GMT-03:00`),
                                      tz = "America/Argentina/Buenos_Aires")) 
  colnames(file_read)[3] <- "Volt, V"
  # if (grep("Temp, ?C", colnames(file_read)[4]) == T) {
  #   colnames(file_read)[4] <- "Temp, ?C"
  #   file_out <- file_read %>%
  #     select(`Date and Time`, `Volt, V`,`Temp, ?C`)
  # } else {
  file_out <- file_read %>%
    select(`Date and Time`, `Volt, V`)
  
  data_out <-   file_out  %>% 
      mutate(`Water Level, meters` =
               case_when(
                 which_catch == 1 ~ -0.03549 + 1.2*`Volt, V`,
                 which_catch == 2 ~ -0.666 + 1.2*`Volt, V`,
                 which_catch == 3 ~ 3.266 -1.28761*`Volt, V`,
                 which_catch == 2 ~ -0.65 + 1.2*`Volt, V`
               )) %>%
      select(`Date and Time`,`Water Level, meters`, logger)
    return(data_out)
  #  }
  browser()
  if (plotit == T) {
    p <-   data_out %>%
      ggplot(aes(`Date and Time`,`Water Level, meters`)) + geom_line() +
      theme_bw()
    print(p)
  }
  return(file_out %>% select(`Date and Time`, `Water Level, meters`))
}



