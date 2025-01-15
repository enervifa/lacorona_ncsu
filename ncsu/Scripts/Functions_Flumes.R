# Process flumes data
# not tested
# tested

require(tidyverse)
require(plotly)


#######  FIRST FUNCTION RUN
#file_path is the directory of Process file

#First function to run
#process_flume_data (file_path = "C:/Users/Lenovo/Downloads/SampleFiles3")
#Second function to run
#quick_check_plot (file_path = "C:/Users/Lenovo/Downloads/SampleFiles3")
#OTIP_EM061523.csv
#precipitacionescorona2023

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
#' Extract the prefix from a file name
#' 
#' function to extract the prefix from the filename
#' This function only works for the current known filename constructs
#' will need to be checked if construct of file names changes
#' @param file_name character string, name of the file
#' 
#' @example
#' pre_fix <- extract_pre_fix("v31101199.csv")
#' pre_fix
#' 
#' @export
extract_pre_fix <- function(file_name) {
  #browser()
  date_string <- extract_date(file_name)[[2]]
  #browser()
  split_result <- str_split(file_name, as.character(date_string), simplify = T)
  return(split_result[1])
}

##########################################################
#' Identify an instrument
#' 
#' function to identify the instrument tyoe from the filename
#' This function only works for the current known filename constructs
#' will need to be checked if construct of file names changes
#' @param file_name character string, name of the file
#' @param file_path_in file folder, set to default file_path
#' 
#' @example
#' test_instrument <- identify_instrument("v31101199.csv")
#' test_instrument
#' 
#' @export
identify_instrument <- function(file_name, file_path_in = file_path) {
  prefix <- extract_pre_fix(file_name)
  # is there a thing called "vel" in the filename (velocity)
  vel_trigger <- ifelse(grepl("vel",file_name, ignore.case = T) == T,
                        TRUE, FALSE)
  # read first three lines of the file
  grab_lines <- read_lines(file.path(file_path_in,file_name), n_max=3)

  if(vel_trigger == T) {
    instrument <- "ISCO_velocity"
  } else {   
    if (grepl("Volt",grab_lines[2], ignore.case =T)) {
      instrument <- "StevensU12"
    } else {
      instrument <- ifelse((grepl("p", prefix, ignore.case = T) == T |
                grepl("S", prefix, ignore.case = T) == T), 
                "HOBOU20", "ISCO_nivel")
    }
  }
  return(instrument)
}



#########################################
#' read manual user input
#' 
#' This function reads in the file names from manual user input
#' 
#' @param file_path a character string giving the path for the files
#'
#' @export
read_flume_files <- function(file_path, manual = T){
  # Create empty lists to store user-provided file names and paths
  user_file_names <- list()
  user_file_paths  <- list()
  file_prefix <- list()
  file_date <- list()
  instrument <- list()
  # Prompt the user for file names until they press Enter
  while (TRUE) {
    # Prompt the user for a file name
    file <- readline("Enter a file name (or press Enter to finish): ")
    # Check if the user pressed Enter (i.e., entered an empty string)
    if (file == "") {
      break  # Exit the loop if the user pressed Enter
    }
    
    # Add the user-provided file name to the user_file_names list
    user_file_names <- append(user_file_names, file)
    # Add the corresponding file path to the user_file_paths list
    # use in_path from input if dirname not specified in the input
    if (dirname(file) == ".") {
      user_file_paths <- append(user_file_paths, file_path)
    } else user_file_paths <- append(user_file_paths, dirname(file))
    
    # Extract the file prefix, date and instrument
    file_prefix <- append(file_prefix, extract_pre_fix(file))
    file_date <- append(file_date, extract_date(file))
    instrument <- append(instrument, identify_instrument(file, 
                                                         file_path_in = file_path))
    
    # inform the user
    cat("Processing file:", file, "\n")
    
  }
  #browser()
  # create an export list
  user_file_data <- list(names = list_c(user_file_names) , 
                         paths = list_c(user_file_paths), 
                         prefix = list_c(file_prefix), 
                         date = file_date[[1]], 
                         instrument = list_c(instrument))
  
  
  # Return the list of file data
  return(user_file_data)
}

#########################################
#' read automatic list of files
#' 
#' This function automatically read list of files names from a directory
#' 
#' @param file_path a character string giving the path for the files
#' @param file_ext a character string giving the extension of the files
#'
#' @export

read_list_flume <- function(file_path, file_ext = ".csv") {
  # this works if all files are in the same directory
  #browser()
  user_file_list <- dir(file_path, pattern = file_ext)
  user_path <- file_path
  # Extract the file prefix, date and instrument
  file_prefix <- map_chr(user_file_list, 
                        extract_pre_fix)
  file_date <- list_c(lapply(map(user_file_list, extract_date),'[[',1))
  #browser()
  instrument <- map_chr(user_file_list, identify_instrument, 
                        file_path_in = file_path)
  

  # return same structure as read_user_input
  return(list(names = user_file_list, 
              paths = rep(user_path, length(user_file_list)),
              prefix = file_prefix,
              date = file_date,
              instrument = instrument))
}


############FIRST FUNCTION PROCESS DATA####################

# #If you are not in a folder with all files to be processed e.g Flumes/Hobo20
# # You can use process_flume_data to define a function to collect user input for file names
# #instruments Hobo20, StevensU12
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
#     
#     # Add the file data to the user_file_data list
#     user_file_data <- append(user_file_data, list(list(ins = instrument,
#                                                        file_name = file,
#                                                        # prefix = file_prefix,
#                                                        date = file_date,
#                                                        full_path = full_file_path,
#                                                        which_catch = catchment)))
#   }
#   
#   # Return the list of file data
#   return(user_file_data)
# }





################ SECOND function to process files and save individual plots############

######################################################################################
#' process list of files and save output
#' 
#' This is the "second" function to process the files in the directory
#' generate the test plots and save the plots
#' 
#' @param file_info a list of file  information
#' @param output_dir path to store the output data frames and plots
#' 
#' test_info <- read_file_manual("../flumes")
#' input: "S1110119.csv", followed by 'enter'
#' process_file_list(test_info, "../flumes/processed)
#' 
#' @export
process_file_list <- function(file_info, out_path) {
  
  output_list <- list()
  browser()
  for(i in 1:length(file_info$names)) {
    #browser()
    output_list[[i]] <- file_process(file_info$names[i],file_info$paths[i], 
                                     file_info$instrument[i], output_dir = out_path)
    
  }

  return(output_list)
}


##################################
#' file process function
#' 
#' @param  file_name name of the file to process
#' @param  file_path path of the file to process
#' @param instrument instrument type
#' 
#' @export
file_process <- function(file_name, file_path, instrument, output_dir) {
      if (!dir.exists(output_dir)) {  
        # Create the output directory if it doesn't exist
        dir.create(output_dir, showWarnings = FALSE)
      }
      #browser()
      if (instrument == "HOBOU20") {
        # Process and plot files from HOBO20 in V1V2, and "v3p"," V4p" from V3V4
        #browser()
        out_plot <- read_hobou20(filename = file_name, 
                                 input_dir = file_path ,plotit = T,
                                outdir = output_dir) 
      } else {
        if (instrument == "StevensU12")  {
          #browser()
          prefix <- extract_pre_fix(file_name)
          out_plot <- read_stevens(file_name, prefix, file_path, plotit = T,
                                  outdir = output_dir)
        } else {
          if (instrument =="ISCO_velocity") vel_trigger = T else vel_trigger = F
          out_plot <- read_isco(file_name, file_path, plotit = T, 
                                velocity = vel_trigger,
                                outdir = output_dir)
          
        }
      }

      return(out_plot)
}

# 
# 

##################################################
#' Read processed flume file
#' 
#' Function to read the processed flume file back into a dataframe
#' @param name the file name to read in
#' @aram path_to_file any additional path, defaults to "."
#' @example 
#' df1 <- read_otip_file(name = "r4111219_processed.csv", 
#'                    path_to_file = "../Flumes/processed")
#' 
#' @export
read_flume_output <- function(name, path_to_file="."){
  data_out <- read_csv(paste0(path_to_file,"/",name))
  # make sure dates are working
  data_out <- data_out %>%
    mutate(`Date and Time` = ymd_hms(Date, tz = "America/Argentina/Buenos_Aires"))
  return(data_out)
}

###############################################
#'Flume Hobo plotting function
#'
#'Function to plot the flume results for the hobo data logger
#'@param df a data frame/tibble as input
#'@param ggtitle_text_in character, the text for the plot title
#'
#'@return a ggplot object
#'
#'@examples 
#'NA
#'
#'@export
flume_hobo_plot <- function(df, ggtitle_text_in) {
  p1 <- df %>%
    na.omit() %>%
    pivot_longer(cols = `Temp, ?C`:`Water Level, meters`,
                 names_to = "Measures", values_to ="values") %>%
    ggplot(aes(`Date and Time`,values, colour = Measures)) +
    geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")+
    ggtitle(ggtitle_text_in) + theme_classic()
  
  return(p1)
}

###############################################
#'Flume Stevens plotting function
#'
#'Function to plot the flume results for the hoboU12- Stevens logger
#'@param df a data frame/tibble as input
#'@param ggtitle_text character, the text for the plot title
#'
#'@return a ggplot object
#'
#'@examples 
#'NA
#'
#'@export
flume_stevens_plot <- function(df, ggtitle_text_in) {
  p1 <-   df %>%
      ggplot(aes(`Date and Time`,`Water Level, meters`)) + geom_line() +
      theme_classic() + ggtitle(ggtitle_text_in)
  # return the plot object
  return(p1)
}

###############################################
#'Flume isco plotting function
#'
#'Function to plot the flume results for the hoboU12- Stevens logger
#'@param df a data frame/tibble as input
#'@param ggtitle_text character, the text for the plot title
#'@param velocity whether velocity needs to be plotted
#'
#'@return a ggplot object
#'
#'@examples 
#'NA
#'
#'@export
flume_isco_plot <- function(df, ggtitle_text, velocity_in) {
  if (velocity_in) {
    p1 <- df %>%
      na.omit() %>%
      ggplot(aes(`Date and Time`,`Velocity (m/s)`)) + geom_line() +
      theme_classic() + ggtitle(ggtitle_text)
    return(p1)
  } else {
    p1 <- df %>%
      na.omit() %>%
      ggplot(aes(`Date and Time`,`Level (ft)`)) + geom_line() +
      theme_classic() + ggtitle(ggtitle_text)
    return(p1)
  }
}

# --- HOBOU20 ----
################
#' read in the Hobou20 file output
#' 
#' @param filename
#' @param input_dir
#' @param coltypes what columntypes should be read in,
#'   defaults to cols("d","c","d","d","d","d","c","c","c","c")
#' @param skip how many rows to skip defaults to 1
#' @param plotit logical switch whether to plot the result
#' @param outdir where to store the output (plot)
#' 
#' @export
read_hobou20 <- function(filename, input_dir ,
                         coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                         skip = 1, plotit = F, outdir = output_dir) {
  browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  file_read <- file_read %>%
    mutate(`Date and Time` = force_tz(mdy_hms(`Date Time, GMT-03:00`),
                                      tz = "America/Argentina/Buenos_Aires")) 
  colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, ?C",
                                "Bar Pressure kPa",
                                "Water Level, meters")
  ggtitle_text <- paste("Flume Data Quick Check (File Name:", filename,")")
  
  file_out <- file_read %>%
    select(`Date and Time`, `Temp, ?C`,
           `Water Level, meters`)
  # write file to directory  
 # browser()
  location <- str_locate(filename, ".csv")
  filename_out <- str_sub(filename, 0,location[1]-1)
  
  write_csv(file_out %>% select(`Date and Time`,
                                `Water Level, meters`), 
            paste0(outdir,"/",filename_out, "_processed.csv"))
  
  if (plotit == T) {
    p <- flume_hobo_plot(file_out, ggtitle_text)
    return(p)

  } else {
    return(file_out %>% select(`Date and Time`,
                               `Water Level, meters`))
    
  }
  
}

############## --- HOBOU12--- Stevens
#'
#'function to read in the Stevens HoboU12 logger data
#' Now returns a ggplot object if plotit == T
#' 
#' @param filename name of file to read in
#' @param file_prefix the extracted file_prefix indicating the flume
#' @param input_dir input directory to read filefrom
#' @param skip numbe rof lines to skip
#' @param plotit  plotting flag
#' @param outdir the output directory defaults to "processed"
#' 
#' @export

read_stevens <- function(filename, file_prefix, input_dir,
                         skip = 1, plotit = F, outdir = "../flumes/processed") {
  #browser()
  # messy, but currently the only way I can do this
  if(str_to_lower(file_prefix) == "v1") coltypes <- cols("d","c","d","d","c","c","c","c","c")
  if(str_to_lower(file_prefix) == "v2") coltypes <- cols("d","c","d","d","c","c","c")
  if(str_to_lower(file_prefix) == "v3") coltypes <- cols("d","c","d","c","c","c","c","c")
  if(str_to_lower(file_prefix) == "v4") coltypes <- cols("d","c","d","c","c","c")
  
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  #browser()
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
               file_prefix == "V1" ~ -0.03549 + 1.2*`Volt, V`,
               file_prefix == "V2" ~ -0.666 + 1.2*`Volt, V`,
               file_prefix == "V3" ~ 3.266 -1.28761*`Volt, V`,
               file_prefix == "V4" ~ -0.65 + 1.2*`Volt, V`
             ))
  
  data_out <- data_out %>%
    select(`Date and Time`,`Water Level, meters`)
  # write file to output directory
  location <- str_locate(filename, ".csv")
  filename_out <- str_sub(filename, 0,location[1]-1)
  
  write_csv(data_out, 
            paste0(outdir,"/",filename_out, "_processed.csv"))
  
  #  }
  ggtitle_text <- paste("Flume Data Quick Check (File Name:", filename,")")
  
  #browser()
  if (plotit == T) {
    p <- flume_stevens_plot(data_out, ggtitle_text)
    return(p)
  } else {
  return(data_out)
  }
}



############ --- read_ISCO ----
#' function to read ISCO files
#' 
#' distighuishes between velocity and "nivel"
#' @param filename name of the file to process
#' @param input_dir input directory
#' @param coltypes column types default set to cols("c","i","i")
#' @param skip number of lines to skip
#' @param plotit flag for plotting
#' @param velocity whether or not velocity should be read in
#' 
#' @export
read_isco <- function(filename, input_dir , 
                      coltypes = cols("c","i","i"),
                      skip = 7, plotit = F, velocity = vel_trigger,
                      outdir = output_dir) {
  browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        col_names =F,
                        skip = skip, col_types = coltypes)

  if (velocity) {
    colnames(file_read) <- c("Date and Time", "Sample",
                             "Velocity (m/s)")
    file_out <- file_read %>%
      mutate(`Date and Time` = time_convert(`Date and Time`))  %>%
      mutate(`Velocity (m/s)` = as.numeric(paste(Sample, `Velocity (m/s)`, sep = ".")))
    data_out <- file_out %>% select(`Date and Time`,`Velocity (m/s)`)
    
  } else {
    colnames(file_read) <- c("Date and Time", "Sample",
                             "Level (ft)")
    file_out <- file_read %>%
      mutate(`Date and Time` = time_convert(`Date and Time`))  %>%
      mutate(`Level (ft)` = as.numeric(paste(Sample, `Level (ft)`, sep = ".")))
    data_out <- file_out %>% select(`Date and Time`,`Level (ft)`)
    
  }

  # write file to output directory
  location <- str_locate(filename, ".csv")
  filename_out <- str_sub(filename, 0,location[1]-1)
  
  write_csv(data_out, 
            paste0(outdir,"/",filename_out, "_processed.csv"))

  ggtitle_text <- paste("Flume Data Quick Check (File Name:", filename,")")
  #browser()
    
  if (plotit == T) {
    p <- flume_isco_plot(data_out, ggtitle_text, velocity)
    return(p)
  } else return(data_out)
}

# auxillary function to deal with a.m. and p.m.

time_convert <- function(text_input) {
  text_out <- str_sub(text_input,1,19)
  text_out <- ifelse(str_detect(text_input, "a.m.") == T,
                     str_c(text_out," AM"),
                     str_c(text_out," PM"))
  # browser()
  text_out <- parse_date_time(text_out,
                              "dmy IMS Op")
  # tz is not working in R4.3.0, unclear when this will be fixed                              
  # "dmy IMS Op",
  # tz = "America/Argentina/Buenos_Aires")
  return(text_out)
}
