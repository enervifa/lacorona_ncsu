library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyverse)
library(htmlwidgets)

# Specify the file and output directories
file_path <- "C:/Users/Lenovo/Downloads/SampleFiles3/SampleFiles_test/T061523/Flumes_pro"
read_dir<-"C:/Users/Lenovo/Downloads/SampleFiles3/SampleFiles_test/T061523/Flumes_pro"
output_dir <- "C:/Users/Lenovo/Downloads/SampleFiles3/SampleFiles_test/T061523/Flumes_pro/output_plots"

# Call the function to collect user input and process files

#V1,Hobo20,H20S1061523.csv,H20V1061523.csv

user_file_data<-list()
plot_list <- list()

user_file_data <- process_flume_data(file_path)

# # Create a list to store processed data for each prefix
# processed_data_all<- list() #
# # Iterate through the user_file_data list
# for (file_info in user_file_data) {
#   # Process and plot files based on conditions
#   processed_data <- process_and_plot_files(file_info)
#   # Store the processed data in the list
#   key <- paste(file_info$file_name, file_info$wich_catch, file_info$ins, sep = "_")
#   processed_data_all[[key]] <- processed_data
#   plot_list
# }

###iterarte to read, plot and save plots 
for (file_info in user_file_data) {
  # Process and plot files based on conditions
  processed_data <- process_and_plot_files(file_info)
key <- paste(file_info$file_name, file_info$which_catch, file_info$ins, sep = "_")
#browser()
processed_data_all[[key]] <- processed_data$data  # Store processed data
#browser()
plot_list <- processed_data$plots  # Store plots
}

#save the plots in .html
combined_html_filename <- file.path(output_dir, "combined_plots.html")
htmltools::save_html(plot_list, combined_html_filename)

#now read listed flies and plot for every flume, and save in .html with rainfall data




############FIRST FUNCTION PROCESS DATA####################

#If you are not in a folder with all files to be processed e.g Flumes/Hobo20
# You can use process_flume_data to define a function to collect user input for file names
#instruments Hobo20, StevensU12
# process_flume_data <- function(file_path) {
#   # Create empty lists to store user-provided file data
#   user_file_data <- list()
#   
#   # Prompt the user for file names until they press Enter
#   while (TRUE) {
#     # Prompt the user for a file name
#     catchment <- readline("Enter catchment name V1, V2, V3 or V4 (or press Enter to finish): ")
#     instrument <- readline("Enter a instrument name (or press Enter to finish): ")
#     file <- readline("Enter a file name (or press Enter to finish): ")
#     # Check if the user pressed Enter (i.e., entered an empty string)
#     if (instrument == "" ) {
#       break  # Exit the loop if the user pressed Enter
#     }
#     
#     # Construct the full file path
#     full_file_path <- file.path(file_path, file)
#     
#     # # Extract the file prefix first two letters
#     # file_prefix <- substr(file, 1, 2)
#     file_date <- substr(file, 3, 8)
#     #nchar (total nmr of characters count number -4chart (.csv) )
#     #list all files
#     #search .csv.csv in string
#     #read name and need to clean csv.csv files 
#     #Add the file data to the user_file_data list
#     
#     user_file_data <- append(user_file_data, list(list(ins = instrument,
#                                                        file_name = file,
#                                                       # prefix = file_prefix,
#                                                        date = file_date,
#                                                        full_path = full_file_path,
#                                                        which_catch = catchment)))
#   }
#   
#   # Return the list of file data 
#   return(user_file_data)
# }

user_file_data <- list()


# Define a function to collect user input for catchment, instrument, and files
process_flume_data <- function(file_path) {
  while (TRUE) {
    # Prompt the user for catchment, instrument, and files
    input_string <- readline("Enter catchment, instrument, and files (e.g., V1, Hobo20, file1.csv, file2.csv) (or press Enter to finish): ")
    
    # Check if the user pressed Enter (i.e., entered an empty string)
    if (input_string == "") {
      break  # Exit the loop if the user pressed Enter
    }
    
    # Split the input string into parts using a comma as the separator
    input_parts <- strsplit(input_string, ",")[[1]]
    
    # Check if there are at least two parts (catchment and instrument)
    if (length(input_parts) >= 2) {
      catchment <- trimws(input_parts[1])  # Remove leading/trailing spaces
      instrument <- trimws(input_parts[2])  # Remove leading/trailing spaces
      
      # Loop through the remaining parts (files)
      for (i in 3:length(input_parts)) {
        file <- trimws(input_parts[i])
        # Find the position of the last dot (.) in the file name
        last_dot_position <- max(gregexpr("\\.", file)[[1]])
        if (last_dot_position > 0) {
          file_date <- substr(file, last_dot_position - 6, last_dot_position - 1)
        } else {
          # Handle cases where the date (day) cannot be extracted
          file_date <- NA
        }
        
        # Construct the full file path
        full_file_path <- file.path(file_path, file)
        
        # Create a list for the current file
        current_file_data <- list(
          which_catch = catchment,
          ins = instrument,
          file_name = file,
          date = file_date,
          full_path = full_file_path
        )
        
        # Append the current file data to user_file_data
        user_file_data <- append(user_file_data, list(current_file_data))
      }
    } else {
      cat("Invalid input format. Please enter catchment, instrument, and at least one file separated by commas.\n")
    }
  }
  
  # Return the list of file data
  return(user_file_data)
}


processed_data_list<-list()
################ SECOND function to process files and save individual plots############
process_and_plot_files <- function(file_info) {
  filename <- file_info$file_name
  file_path <- file_info$full_path
#  browser()
  # Perform data transformations and analysis based on conditions
  file_catch <- file_info$which_catch
  file_ins <- file_info$ins
#  browser()
  if (file_ins %in% c("Hobo20")) {
  # Process and plot files from HOBO20 in V1V2, and "v3p"," V4p" from V3V4
  #  browser()
    result_hobo20<-read_hobou20(filename = filename, input_dir = read_dir ,plotit = T) 
    # Create a tag for the processed data
    result_hobo20tag <- result_hobo20$tolist %>%
      mutate(ins = "Hobo20") 
    result_hobo20tag <- result_hobo20tag%>%
      mutate(catch = file_info$which_catch)
        # Store the processed data and tag in the list
    processed_data_list[[filename]] <- list(data = result_hobo20tag)
    plot_list[[filename]] <- list(plots =result_hobo20$plot)
 } else if (file_ins %in% c("StevensU12"))  {
# browser()
   result_stevens<-read_stevens(file_info, input_dir = read_dir, plotit = T)
 # Create a tag for the processed data
#   browser()
 result_stevenstag <- result_stevens$tolist %>%
   mutate(ins = "StevensU12") 
 result_stevenstag <-result_stevenstag%>%
   mutate(catch = file_info$which_catch)
# browser()
 # Store the processed data and tag in the list
 browser()
 processed_data_list[[filename]] <- list(data = result_stevenstag)
 plot_list[[filename]] <- list(plots =result_stevens$plot)
 
 } else if (file_ins %in% c("ISCO"))  {
 #  browser()
   result_isco <- read_isco(file_info, input_dir = read_dir, plotit = T)
 #  browser()
   # Create a tag for the processed data
   result_iscotag <- result_isco$tolist %>%
     mutate(ins = "ISCO") 
   
  #   browser() 
     result_iscotag <- result_iscotag %>%
     mutate(catch = file_info$which_catch)
   # Store the processed data and tag in the list
 #    browser()  
   processed_data_list[[filename]] <- list(data = result_iscotag)
   plot_list[[filename]] <- list(plots =result_isco$plot)

 }
  
 else if (file_ins %in% c("ISCOV"))  {
 # browser()
  result_iscoV <- read_isco_velocity(file_info, input_dir = read_dir, plotit = T)
#  browser()
  # Create a tag for the processed data
  result_iscoVtag <- result_iscoV$tolist %>%
    mutate(ins = "ISCOV") 
  result_iscoVtag <-result_iscoVtag%>%
    mutate(catch = file_info$which_catch)
  # Store the processed data and tag in the list
  processed_data_list[[filename]] <- list(data = result_iscoV)
  plot_list[[filename]] <- list(plots =result_iscoV$plot)
 }  
  
  
  # # Save individual plot as PNG
  # plot_filename <- file.path(output_dir, paste0(filename, "_plot.png"))
  # ggsave(filename = plot_filename, plot = p, device = "png")
 # return(processed_data_list)
   return(list(data=processed_data_list, plots = plot_list))

}


# Combine the plots into a single HTML file
html_output <- tagList(plot_list)

# Save the HTML file
htmlwidgets::saveWidget(html_output, "plots.html")
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
  ggtitle_text <- paste("Flumes Quick Check (File Number:", filename, file_ins,")")
  
  file_out <- file_read %>%
    select(`Date and Time`, `Temp, ?C`,
           `Water Level, meters`)
#  browser()
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Temp, ?C`:`Water Level, meters`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")+
      ggtitle(ggtitle_text)
   
    #add plotly
    #Append file to a list of plots
     plothobo20<-ggplotly(p)
  #   plot_list[[filename]] <- plothobo20
     
#    browser()
    # Save individual plot as PNG
    plot_filename <- file.path(output_dir, paste0(filename, "_plot.png"))
    ggsave(filename = plot_filename, plot = p, device = "png",width = 10, height = 8, dpi = 300)
    

  }
  
 #return(file_out %>% select(`Date and Time`,
 #                            `Water Level, meters`))
  return(list(tolist=(file_out %>% 
                        select(`Date and Time`,`Water Level, meters`)), plot=plothobo20))
}

# --- HOBOU12--- Stevens

# read the stevens logger
read_stevens <- function(file_info, input_dir,
                         skip = 1, plotit = F) {
#  browser()
  filename <- file_info$file_name
#  browser()
  # messy, but currently the only way I can do this
  if(grepl("V1",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c","c","c")
  if(grepl("V2",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","d","c","c","c")
  if(grepl("V3",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","c","c","c","c","c")
  if(grepl("V4",substr(filename,1,2), ignore.case =T)) coltypes <- cols("d","c","d","c","c","c")
  
# browser()
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
 # browser()
  ggtitle_text <- paste("Flumes Quick Check (File Number:", filename, file_ins,")")
  
  file_out <- file_read %>%
  select(`Date and Time`, `Volt, V`)
# browser()
  data_out <-   file_out  %>% 
      mutate(`Water Level, meters` =
               case_when(
                 file_info$which_catch == "V1" ~ -0.03549 + 1.2*`Volt, V`,
                 file_info$which_catch == "V2" ~ -0.666 + 1.2*`Volt, V`,
                 file_info$which_catch == "V3" ~ 3.266 -1.28761*`Volt, V`,
                 file_info$which_catch == 'V4' ~ -0.65 + 1.2*`Volt, V`
               )) 
#    browser()
 #   return(file_out)
  #  }
# browser()
  if (plotit == T) {
    p <-   data_out %>%
      ggplot(aes(`Date and Time`,`Water Level, meters`)) + geom_line() +
      theme_bw()+ggtitle(ggtitle_text)
   plotstevensu12<- ggplotly(p)
   
    
  # plot_list[[filename]] <-  plotstevensu12
   
    # Save individual plot as PNG
    plot_filename <- file.path(output_dir, paste0(filename, "_plot.png"))
    ggsave(filename = plot_filename, plot = p, device = "png",width = 10, height = 8, dpi = 300)
    

    
  }
#browser()
  #return(data_out)
return(list(tolist=data_out, plot=plotstevensu12))
}

#-----------ISCO----------
read_isco <- function(file_info, input_dir , 
                      coltypes = cols("c","i","i"),
                      skip = 7, plotit = F) {
#  browser()
  filename <- file_info$file_name
#  browser()
  
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        col_names =F,
                        skip = skip, col_types = coltypes)
  colnames(file_read) <- c("Date and Time", "Sample",
                       "Level (ft)")
 # browser()  
  file_out <- file_read %>%
    mutate(`Date and Time` = time_convert(`Date and Time`)) %>%
    mutate(`Level (ft)` = as.numeric(paste(Sample, `Level (ft)`, sep = ".")))
#  browser()
  
  ggtitle_text <- paste("Flumes Quick Check (File Number:", filename, file_ins,")")
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Sample`:`Level (ft)`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")+ ggtitle(ggtitle_text)
   plotisco<-ggplotly(p)
    
    plot_filename <- file.path(output_dir, paste0(filename, "_plot.png"))
    ggsave(filename = plot_filename, plot = p, device = "png",width = 10, height = 8, dpi = 300)
   # htmlwidgets::saveWidget(as_widget(pg), file.path(output_dir, "index.html"))
  }

#  browser()  
  return(list(tolist=file_out, plot=plotisco))

   #%>% select(`Date and Time`,`Level (ft)`))
}



time_convert <- function(text_input) {
  text_out <- str_sub(text_input,1,19)
  text_out <- ifelse(str_detect(text_input, "a.m.") == T,
                     str_c(text_out," AM"),
                     str_c(text_out," PM"))
  text_out <- parse_date_time(text_out,
                              "dmy IMS Op",
                              tz = "America/Argentina/Buenos_Aires")
  return(text_out)
}

# # testing
# read_dir <- "SampleFiles/Flumes/V1V2/ISCOSampler"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test_isco <- read_isco(filename = filenames[1], input_dir = read_dir,
#                   plotit = T)
# head(test_isco)

# --- ISCO velocity only V3 and V4  ----
read_isco_velocity <- function(filename, input_dir , 
                               coltypes = cols("c","i","i"),
                               skip = 7, plotit = F) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        col_names =F,
                        skip = skip, col_types = coltypes)
  colnames(file_read) <- c("Date and Time", "Sample",
                           "velocity (ft/s)")
  file_out <- file_read %>%
    mutate(`Date and Time` = time_convert(`Date and Time`)) %>%
    mutate(`velocity (ft/s)` = as.numeric(paste(Sample, `velocity (ft/s)`, sep = ".")))
  
  ggtitle_text <- paste("Flumes Quick Check (File Number:", filename, file_ins,")")
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Sample`:`velocity (ft/s)`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")+ ggtitle(ggtitle_text)
    print(p)
    
    plotiscov<-ggplotly(p)
 
  }
#  return(file_out %>% select(`Date and Time`,`velocity (ft/s)`))
  return(list(tolist=(file_out %>% select(`Date and Time`,`velocity (ft/s)`)), plot=plotiscov))
}

