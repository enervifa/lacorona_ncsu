# File to read in the groundwater well data
# function is different from Flume Hobo function

# Functions to read in all the water and well data

require(tidyverse)
#require(lubridate)

##################################################
#' Read well file
#' 
#' Function to read well file
#' @param filename the file name to read in
#' @param input_dir any additional path, defaults to "."
#' @param coltypes column types, has a default to match current files
#' @param skip defaults to 1
#' @param plotit Switch whether or not to plot the output
#' @example 
#' df1 <- read_hobo_well(filename = "N1111219.csv", 
#'                    input_dir = "../Wells/Automatic")
#' 
#' @export
read_hobo_well <- function(filename, input_dir = "."  , 
                      coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                      skip = 1, plotit = F) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  
  colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, ?C",
                                "Bar Pressure kPa",
                                "Water Level, meters")
  file_read <- file_read %>%
    mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
  
  file_out <- file_read %>%
    select(`Date and Time`,`Abs Pressure kPa`, `Temp, ?C`,
                                `Water Level, meters` ) %>%
    # remove missing values
    na.omit() %>%
    # remove first two hours as these are often wrong
    filter(`Date and Time` > min(`Date and Time`) + 2)
  
  
  #write_well(file_out,filename)
  
  if (plotit == T) {
    p <- plot_well(file_out)
    #print(p)
  } else p <- NA
  return(list(file = file_out, plot = p))
}


#######################################
#' plotting function
#' 
#' Function to plot the well output
#' @param df dataframe to plot
#' @param baro switch (true/false) for baro fiel plotting
#' @return a ggplot structure
#' @example
#' df1 <- read_hobo_well(filename = "N1111219.csv", 
#'                    input_dir = "../Wells/Automatic")
#' p_out <- plot_well(df1) 
#' 
#' @export
plot_well <- function(df, baro = FALSE) {
  if (baro  == F) {
    p <- df %>%
      na.omit() %>%
      pivot_longer(cols = 2:ncol(df),
                   names_to = "Measures", values_to ="values") %>%
      ggplot(aes(`Date and Time`,values, colour = Measures)) +
      geom_line() + facet_wrap(~Measures, ncol = 2, scales = "free") + 
      theme_bw()
    
  } else {
    p <- df %>%
      na.omit() %>%
      ggplot(aes(`Date and Time`,`Barom Pressure kPa`)) +
      geom_line(colour = "blue") + theme_bw()
  }
  return(p)
}

##################################
#' Write processed file
#' 
#' Write the processed file to a path
#' @param df dataframe to write
#' @param filename_in filename to write processed
#' @param path_out directory to write the file
#' @return nothing, writes file to a directory
#' @example
#' 
#' @export
write_well <- function(df, filename_in, path_out = "../Wells/Automatic/Processed") {
  write_csv(df, paste0(path_out,"/",filename_in,"_processed.csv"))
}

##################################################
#' Read baro file
#' 
#' Function to read the baro file
#' @param filename the file name to read in
#' @param input_dir any additional path, defaults to "."
#' @param coltypes column types, has a default to match current baro file
#' @param skip defaults to 1
#' @return a list with the file data and a plotting structure
#' @example 
#' df1 <- read_hobo_baro(filename = "N11111219.csv", 
#'                    input_dir = "../Wells/Automatic")
#' 
#' @export
read_hobo_baro <- function(filename, input_dir = "."  , 
                           coltypes = cols("d","c","d","d","d","d","c","c","c","c"),
                           skip = 1, plotit = T) {
  #browser()
  file_read <- read_csv(paste(input_dir,filename,sep="/"),
                        skip = skip, col_types = coltypes)
  
  colnames(file_read)[3:6] <- c("Abs Pressure kPa", "Temp, ?C",
                                "Barom Pressure kPa",
                                "Water Level, meters")
  file_read <- file_read %>%
    mutate(`Date and Time` = mdy_hms(`Date Time, GMT-03:00`)) 
  
  file_out <- file_read %>%
    select(`Date and Time`,`Barom Pressure kPa`)

  if (plotit == T) {
    p <- plot_well(file_out, baro = T)
    #print(p)
  } else p <- NA
  return(list(file = file_out, plot = p))
}

#############################
#' merge baro and well at hourly time steps
#' 
#' Merge the baro pressure and well data to process
#' @param baro_df file with baro pressure data
#' @param well_df file with waterlevel/pressure data
#' @return a list with a df with the merged data and a plotting structure
#' @example 
#' 
#' 
#' @export
Merge_baro_well <- function(baro_df, well_df) {
  # First we need to summarise the barometric pressure over the hour that the
  # well measurements are taken
 # browser()
  baro_df_test <- baro_df %>%
    mutate(Datehour = round(`Date and Time`, unit = 'hour')) %>%
    group_by(DateTime = Datehour) %>%
    summarise(`Barom Pressure kPa` = mean(`Barom Pressure kPa`, na.rm =T))
  print(baro_df_test)
  
  merge_dfs <- left_join(baro_df_test, well_df %>% 
                           mutate(DateTime = round(`Date and Time`, 
                                                   unit = 'hour')))
  # make a quick plot
  p <- merge_dfs %>% ggplot(aes(`Barom Pressure kPa`, `Abs Pressure kPa`)) + 
    geom_point() + geom_smooth(method = "lm") + theme_bw()
  
  return(list(data = merge_dfs, plot = p))
  
}

#############################
#' extract well data in document
#' 
#' extract the section from the document that has well data
#' put into a tibble
#' There are two sections in the document
#' @param well_data_start start and finish locations in the text with the data
#' @param text_in all the text from the document
#' @param text_string text_string uniquely indicating a row of well data ("N.")
#' @return a tibble with the well ids and depths
#' @example
#' text_in_file <- readtext(paste("../Wells/Manual", 
#'      "Planilla cuenca 111219.docx", sep ="/"), 
#'      encoding = "utf8")$text
#'    well_data_section <- doc_data_find(text_in = text_in_file,
#'                                      feature_1 = "Wells",
#'                                      feature_2 = "Rain\n")   
#' @export 
extract_well_sec <- function(well_data_start, text_in, text_string) {
  well_data <- str_sub(text_in, well_data_start[1]+2,well_data_start[2]-1)
  # the character string with the well data
  well_text <- str_split(well_data, "\n")
  # the names/locations of the wells
  grab <- unlist(str_locate(well_text[[1]], text_string))[,1]
  # the locations of the data that is not the well names
  non_wells <- (as.numeric(attr(na.omit(grab),"na.action")))
  # the locations for all the well names
  loc_well <- seq(1:length(well_text[[1]]))[-non_wells]
  # extract the well names and the data
  grab_well <- tibble(Well = well_text[[1]][loc_well], 
                      Depth = well_text[[1]][loc_well+1])
  # now need to extract this for both dates
  return(grab_well)
}


#############################
#' find data in document
#' 
#' Find the section from the document that has feature data
#' @param text_in all the text from the document
#' @param feature_1 Feature that you want to find
#' @param feature_2 Following feature
#' @return location numbers for the section to extract
#' @example
#'  require(readtext)
#'  # read in the file
#' text_in_file <- readtext(paste("../Wells/Manual", 
#'      "Planilla cuenca 111219.docx", sep ="/"), 
#'      encoding = "utf8")$text
#'    well_data_section <- doc_data_find(text_in = text_in_file,
#'                                      feature_1 = "Wells",
#'                                      feature_2 = "Rain\n")   
#' @export                                      
doc_data_find <- function(text_in, feature_1, feature_2) {
  
  # find the start of the data from the sheet
  feature1_find <- str_locate_all(text_in,feature_1)
  # find the start of the next data, locator in the sheet
  feature2_find <- str_locate_all(text_in,feature_2)
  # the sections of the document with the well data
  data_section <- cbind(feature1_find[[1]][,2],feature2_find[[1]][,1]-1)
  
  return(data_section)
}



# This function might not be needed, extract date from filename
#############################
#' find data in document
#' 
#' Find the dates in the files
#' This function is not very generic,specific to the La Corona files
#' @param text_in all the text from the document
#' @param char_string String indicating location of the dates
#' @return a tibble with the dates 
#' @example
#' text_in_file <- readtext(paste("../Wells/Manual", 
#'      "Planilla cuenca 111219.docx", sep ="/"), 
#'      encoding = "utf8")$text
#' dates <- find_dates(text_in_file, "Data")
#' @export     
find_dates <- function(text_in, char_string = "Data") {
    # find the dates on the sheets
    line_dates <- str_locate_all(text_in,char_string)
    dates <- as.Date(vector(mode = "character"))
    for (i in 1:nrow(line_dates[[1]])) {
      dates[[i]] <- dmy(str_sub(text_in,line_dates[[1]][i,2]+1,
                                line_dates[[1]][i,2]+12))
    }
  return(tibble(dates = dates))
}


#############################
#' read manual well
#' 
#' read the manual well files
#' @param filename the file name to read in
#' @param input_dir any additional path, defaults to "."
#' @return a tibble with the well ids, the measurements and the dates
#' @example
#' well_test <- read_manual_well("Planilla cuenca 111219.docx")
#' 
#' @export
read_manual_well <- function(filename, input_dir = "../Wells/Manual") {
  require(readtext) # https://cran.r-project.org/web/packages/readtext/vignettes/readtext_vignette.html#microsoft-word-files-.doc-.docx
  #browser()  
  # read in the file
  text_in_file <- readtext(paste(input_dir, filename, sep ="/"), 
                           encoding = "utf8")$text
  # find the dates on the sheets
  dates <- find_dates(text_in_file, "Data")
  # line_well <- str_locate_all(text_in_file,"Data")
  # dates <- tibble(dates = rep(ymd(Sys.Date()), nrow(line_well[[1]])))
  # for (i in 1:nrow(line_well[[1]])) {
  #     dates$dates[i] <- dmy(str_sub(text_in_file,line_well[[1]][i,2]+1,
  #                             line_well[[1]][i,2]+12))
  #   }
  #-----------------------------
  # Find the data from the Wells
  well_data_section <- doc_data_find(text_in = text_in_file,
                                     feature_1 = "Wells",
                                     feature_2 = "Rain\n")
  
  well_data <- list()
  # put the well tables in the list
  for (i in 1:nrow(well_data_section)) {
    well_data[[i]] <- extract_well_sec(well_data_start = well_data_section[i,], 
                                       text_in = text_in_file, 
                                       text_string = "N.")
    well_data[[i]] <- well_data[[i]] %>%
      mutate(Date = dates$dates[i])
  }
  #browser()  
  
  # combine the list and output
  Out <- bind_rows(well_data)
  return(Out)


}

#testing
#well_test <- read_manual_well("Planilla cuenca 111219.docx")
#well_test
# # testing
# read_dir <- "SampleFiles/Wells/Automatic"
# filenames <- dir(path = read_dir, pattern = ".csv")
# 
# test_well <- read_hobo_well(filename = filenames[1], input_dir = read_dir,
#                   plotit = T) 
# head(test_well)