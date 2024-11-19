# File to read in manually recorded data in the Planilla *** .docx
# the original doc files first need to be converted to docx
# We used http://www.multidoc-converter.com/en/index.html

# This file attempts to have functions to extract all relevant sections

require(tidyverse)
require(readtext)



#############################
#' extract specific data in document
#' 
#' extract the section from the document that has specific data
#' based on start and finish text position and 
#' @param data_start start and finish locations in the text with the data
#' @param text_in all the text from the document
#' @param text_string text_string uniquely indicating a row of feature data ("N.")
#' @param places1 number of text places to add after data_start (= 2)
#' @param places2 number of text places to go back after finish (= 1)
#' @param split_string character where to split the extracted text string
#' @example
#' text_in_file <- readtext(paste("../Wells/Manual", 
#'      "Planilla cuenca 111219.docx", sep ="/"), 
#'      encoding = "utf8")$text
#' well_data_section <- doc_data_find(text_in = text_in_file,
#'                                      feature_1 = "Wells",
#'                                      feature_2 = "Rain\n")   
#' well_data <- extract_data_sec(data_start = well_data_section[1,]
#'                      text_in = text_in_file, text_string = "N.")
#'                      
extract_data_sec <- function(data_start, text_in, text_string = "N.", 
                             places1 = 2, places2 = 1, 
                             split_string = "\n") {
  data <- str_sub(text_in, data_start[1]+ places1, data_start[2] - places2)
  # split the character string with the well data at "\n"
  extract_text <- str_split(data, split_string)
  # the names/locations of the features
  # #browser()
  # if (length(text_string) > 1) {
  #   foo <- paste0(text_string[1],"|",text_string[2],"|",text_string[3])
  #   grab <- str_detect(extract_text[[1]],foo)
  #   loc_feature <- seq(1:length(extract_text[[1]]))[grab == T]
  # } else {
    grab <- str_detect(extract_text[[1]], text_string)
    # the locations for all the feature names
    loc_feature <- seq(1:length(extract_text[[1]]))[grab == T]
  #}

  
  #browser()
  ## find_times
  sample_times <- extract_times(text_in = extract_text)
  # extract the feature names and the data
  grab_feature <- tibble(Station = extract_text[[1]][loc_feature], 
                      Actual = extract_text[[1]][loc_feature+1])
  if (length(text_string) == 1) { 
    if (text_string != "N.") {
    grab_feature <- grab_feature %>%
      mutate(Module = extract_text[[1]][loc_feature+2])
  }}
  #browser()
  grab_feature <- grab_feature %>%
                      mutate(times = sample_times)
  return(grab_feature)
}


#############################
#' extract_times
#' 
#' function to extract time locations from document
#' 
#' @param text_in
#' @param text_string string to search for, in this case ":"
#' @return a vector of extracted time values
#' @example
#' text_in_file <- readtext(paste("../Wells/Manual", 
#'      "Planilla cuenca 111219.docx", sep ="/"), 
#'      encoding = "utf8")$text
#' well_data_section <- doc_data_find(text_in = text_in_file,
#'                                      feature_1 = "Wells",
#'                                      feature_2 = "Rain\n")
#' Times <- extract_times(well_data_section)   
#' @export
extract_times <- function(text_in, text_string = ":") {
  grab <- unlist(str_locate(text_in[[1]], text_string))[,1]
  # the times to extract
  #browser()
  times <- na.omit(text_in[[1]][grab == 3])
  return(times)
}

# # testing
# text_in_file <- readtext(paste("../Wells/Manual", 
#                                "Planilla cuenca 111219.docx", sep ="/"), 
#                          encoding = "utf8")$text
# well_data_section <- doc_data_find(text_in = text_in_file,
#                                    feature_1 = "Wells",
#                                    feature_2 = "Rain\n")   
# well_data <- extract_data_sec(data_start = well_data_section[1,],
#                               text_in = text_in_file, text_string = "N.")


#############################
#' find data in document
#' 
#' Find the section from the document that has feature data
#' @param text_in all the text from the document
#' @param feature_1 Feature that you want to find
#' @param feature_2 Following feature
#' @return df with text locations
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
  # the sections of the document with the relevant data
  # Flumes are different because there is also a title
  if (feature_1 == "Flumes") {
    data_section <- cbind(feature1_find[[1]][c(2,4),2],feature2_find[[1]][,1]-1)
  } else {
    data_section <- cbind(feature1_find[[1]][,2],feature2_find[[1]][,1]-1)
  }
  
  return(data_section)
}




#############################
#' find dates in document
#' 
#' Find the dates in the files
#' This function is not very generic,specific to the La Corona files
#' @param text_in all the text from the document
#' @param char_string String indicating location of the dates
#' @return vector of dates
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
#' read_manual_file
#' 
#' read the manual entered files
#' @param filename the file name to read in
#' @param feature Main feature of interest, can be c("Flumes", "Sampler", "Wells",
#' "Rain", "Weather")
#' 
#' @param input_dir any additional path, defaults to "../Wells/Manual"
#' @example
#' well_test <- read_manual_well("Planilla cuenca 111219.docx")
#' 
#' @export
read_manual_file <- function(filename, feature, input_dir = "../Wells/Manual") {
  #browser()  
  # read in the file
  text_in_file <- readtext(paste(input_dir, filename, sep ="/"), 
                           encoding = "utf8")$text
  #browser()
  # extract the date from the file name
  Date_file <- dmy(substr(filename,nchar(filename)-12,nchar(filename)-5))
  # Create extraction parameters
  pars <- list(feature2 = case_when(
              feature == "Flumes" ~ "Sampler\n",
              feature == "Sampler" ~ "Wells\n",
              feature == "Wells" ~ "Rain\n",
              feature == "Rain" ~ "Weather\n"
              ),
              char_string =  case_when(
                  feature == "Flumes" ~ c("V.", "S.", "E."),
                  feature == "Sampler" ~ "SMP.",
                  feature == "Wells" ~ "N.",
                  feature == "Rain" ~ "R."
                ))
   #browser()
  #   # find the dates on the sheets
  # dates <- find_dates(text_in_file, "Data")
  #-----------------------------
  # Find the data section in the document
  data_section <- doc_data_find(text_in = text_in_file,
                                     feature_1 = feature,
                                     feature_2 = pars$feature2)
  
  
  out_data <- list()
  #browser()
  # put the well tables in the list
  for (i in 1:nrow(data_section)) {
    out_data[[i]] <- extract_data_sec(data_start = data_section[i,], 
                                       text_in = text_in_file, 
                                       text_string = unique(pars$char_string))
    out_data[[i]] <- out_data[[i]] %>%
      mutate(Date = Date_file)
  }
  #browser()  
  
  # combine the list and output
  Out <- bind_rows(out_data)
  return(Out)
  
}


#############################
#' extract a specific flume data set in document
#' 
#' extract the section from the document that has specific data
#' based on start and finish text position and 
#' @param data_start start and finish locations in the text with the data
#' @param text_in all the text from the document
#' @param text_string text_string uniquely indicating a row of feature data ("V." or "E." or "S.")
#' @param places1 number of text places to add after data_start (= 2)
#' @param places2 number of text places to go back after finish (= 1)
#' @param split_string character where to split the extracted text string
#' @example
#' text_in_file <- readtext(paste("../Wells/Manual", 
#'      "Planilla cuenca 111219.docx", sep ="/"), 
#'      encoding = "utf8")$text
#' well_data_section <- doc_data_find(text_in = text_in_file,
#'                                      feature_1 = "Wells",
#'                                      feature_2 = "Rain\n")   
#' well_data <- extract_data_sec(data_start = well_data_section[1,]
#'                      text_in = text_in_file, text_string = "N.")
#'                      
extract_data_sec <- function(data_start, text_in, text_string = "N.", 
                             places1 = 2, places2 = 1, 
                             split_string = "\n") {}


#testing
# well_test <- read_manual_file(filename = "Planilla cuenca 111219.docx",
#                               feature = "Wells")
# well_test

Flumes_test <- read_manual_file(filename = "Planilla cuenca 111219.docx", 
                                feature = "Flumes")
Flumes_test
