# Process flumes data
# not tested

require(tidyverse)



# Look at the table in github
  # | Data/Instrument                                                  | Extension/Filenames                                                | Scripts to Process      |
  # |-----------------------------------------------------------------|---------------------------------------------------------|-------------------------|
  # | Rain/Automatic rain gauges (R1,R7,EM)                           | .hobo/ R1061523.csv, R7061523.csv, EM061523.csv               | 1_quickcheck_rain.R, 2_cumrain.R |
  # | Rain/Automatic rain gauges and Manual Rain gauges (R2,R3,R4,R5,R6,RV1) | .hobo /R1061523.csv, R7061523.csv, EM061523.csv, .xlsx/Precipitaciones La Corona 2023.xls | 1_quickcheck_rain.R |
  # | Weather/Weather Station                                          | .dat/EM061523.dat                                            | 1_quickcheck_weather.R  |
  # | Flume/Hobou20                                                    |.hobo/  S1061523.csv, S2061523.csv, v3p111219.csv, V4p111219.csv | quickcheckflumes.R      |
  # | Flume/StevensU12                                                 | .hobo/ V4111219.csv | quickcheckflumes.R      |
  # | Flume/ISCO                                                       | .csv/ v3110119csv.csv  V1052523.csv | quickcheckflumes.R      |
  # | Flume/ISCOvel                                                    | .csv/  V3111219VEL.csv, v3110119vel.csv| quickcheckflumes.R      |
  

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
#' 
#' @example
#' test_instrument <- identify_instrument("v31101199.csv")
#' test_instrument
#' 
#' @export
identify_instrument <- function(file_name) {
  prefix <- extract_pre_fix(file_name)
  vel_trigger <- ifelse(grepl("vel",file_name, ignore.case = T) == T,
                        TRUE, FALSE)
  instrument <- ifelse(vel_trigger == T, "ISCO", 
                       ifelse((grepl("p", prefix) == T |
                                grepl("S", prefix) == T), "HOBOU20", "StevensU12"))
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
    instrument <- append(instrument, identify_instrument(file))
    
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
  instrument <- map_chr(user_file_list, identify_instrument)
  

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
  
  for(i in 1:length(file_info$names)) {
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

      if (instrument == "HOBOU20") {
        # Process and plot files from HOBO20 in V1V2, and "v3p"," V4p" from V3V4
        #browser()
        data_df <- read_hobou20(filename = file_name, input_dir = file_path ,plotit = T,
                                outdir = output_dir) 
      } else {
        if (instrument == "StevensU12")  {
          #browser()
          prefix <- extract_pre_fix(file_name)
          data_df <- read_stevens(file_name, prefix, file_path, plotit = T)
        } else {
          data_df <- read_isco(file_name, file_path, plotit = T)
          
        }
      }

      return(data_df)
}

# 
# # Create the output directory if it doesn't exist
# dir.create(output_dir, showWarnings = FALSE)



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
  #browser()
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
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Temp, ?C`:`Water Level, meters`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")+
      ggtitle(ggtitle_text) + theme_classic()
    print(p)
    #browser()
    ggsave(file.path(outdir, filename, "_plot.png"), width = 10, height = 8)

  }
  return(file_out %>% select(`Date and Time`,
                             `Water Level, meters`))
  
}

# --- HOBOU12--- Stevens

# read the stevens logger
read_stevens <- function(filename, file_prefix, input_dir,
                         skip = 1, plotit = F, outdir = output_dir) {
  #browser()
  # messy, but currently the only way I can do this
  if(str_to_lower(file_prefix) == "v1") coltypes <- cols("d","c","d","d","c","c","c","c","c")
  if(str_to_lower(file_prefix) == "v2") coltypes <- cols("d","c","d","d","c","c","c")
  if(str_to_lower(file_prefix) == "v3") coltypes <- cols("d","c","d","c","c","c","c","c")
  if(str_to_lower(file_prefix) == "v4") coltypes <- cols("d","c","d","c","c","c")
  
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  browser()
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
             )) %>%
    select(`Date and Time`,`Water Level, meters`, logger)
  return(data_out)
  #  }
  #browser()
  if (plotit == T) {
    p <-   data_out %>%
      ggplot(aes(`Date and Time`,`Water Level, meters`)) + geom_line() +
      theme_bw()
    print(p)
    ggsave(file.path(outdir, filename, "_plot.png"), width = 10, height = 8)
  }
  return(file_out %>% select(`Date and Time`, `Water Level, meters`))
}

### read ISCO
read_isco <- function(filename, input_dir , 
                      coltypes = cols("c","i","i"),
                      skip = 7, plotit = F, out_dir = output_dir) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        col_names =F,
                        skip = skip, col_types = coltypes)
  colnames(file_read) <- c("Date and Time", "Sample",
                           "Level (ft)")
  file_out <- file_read %>%
    mutate(`Date and Time` = time_convert(`Date and Time`)) %>%
    mutate(`Level (ft)` = as.numeric(paste(Sample, `Level (ft)`, sep = ".")))
  
  if (plotit == T) {
    p <- file_out %>%
      na.omit() %>%
      pivot_longer(cols = `Sample`:`Level (ft)`,
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free")
    print(p)
    png(file.path(outdir, filename, "_plot.png"), width = 10, height = 8)
    print(p)
    dev.off()
  }
  return(file_out %>% select(`Date and Time`,`Level (ft)`))
}




# shared with Functions_Rain.R

#########################################
#' Read data from a file
#' 
#' This function reads the data from the files in the file_list
#' It converts the date column into real dates
#' It select only the required columns
#' 
#' @param name name of a file to be read
#' @param file_path a character string giving the path for the files
#' @param select_col numbers of the columns to select
#'
#' @export
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

#########################################
#' Write a data frame to a file for a specific input file
#' 
#' This function reads the data from the files in the file_list
#' It converts the date column into real dates
#' It select only the required columns
#' 
#' @param name name of the original file to be written
#' @param df the final processed data frame
#' @param input a list of file names and input paths, default given
#' @param output_path optional output path
#'
#' @export
# write otip file
write_otip_file <- function(name, df, input = input, output_path = NULL) {
  # Define the output file path and name tag
  #browser()
  file_path <- input$paths[[which(input$names==name)]]
  #output_path <- paste0(file_path,"/","OTIP_", name)
  output_file <- paste0("OTIP_", name)
  #browser()
  # Save the processed data to a csv file
  if (!is.null(output_path)) {
    write_csv(df, file = paste0(file_path,"/", output_path, "/", output_file), quote = "none")
  } else write_csv(df, file = paste0(file_path,"/", output_file), quote = "none")
  
  return(list(path = file_path, file = output_file))
}

##############################################
#' read otip file
#' 
#' Read in a processed file again
#' @param name name of the file to read
#' @param path_to_file path to the file
#' 
#' @export
read_otip_file <- function(name, path_to_file){
  data_out <- read_csv(paste0(path_to_file,"/",name))
  # make sure dates are working
  data_out <- data_out %>%
    mutate(date = ymd_hms(Date, tz = "America/Argentina/Buenos_Aires")) %>%
    # rename Difference to Event
    rename("Event" = "Difference")
  return(data_out)
}

#########################################
#' difference data
#' 
#' Calculate the differenced event column
#' @param data the dataframe with the data
#' 
#' @export
diff_data <- function(data) {
  data1 <- data%>%
    mutate(Difference = c(0,diff(Event))) %>%
    #ifelse(Event >= lag(Event, default = first(Event)), Event - lag(Event, default = first(Event)), 0))%>%
    select(Date, Difference)
  return(data1)
}

