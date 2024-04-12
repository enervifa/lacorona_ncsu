# Generic data reader
require(tidyverse)
require(lubridate)

# generic read function
read_data <- function(main_type, which_catch,
                      what_to_plot, time_scale = "hour",
                      main_path = "SampleFiles",
                      specific = NULL,
                      fill_missing = F) {
 #browser()
  # interpret the main type to select the folder
    fun <- case_when(
      main_type == "Flumes" ~ "read_flume",
      main_type == "Wells" ~ "read_well",
      main_type == "Rain" ~ "read_rain",
      main_type == "Weather" ~ "read_weather"
    )
  
    if (main_type == "Flumes") {
      # read in the main data
      data_r <- do.call(fun,
                      list(which_catch = which_catch, 
                           main_p = main_path, specific = specific))
      data_r <- bind_rows(data_r)
    
    
      if (fill_missing == T) {
        ## -----------------------------------------
        ## in here do something with fill_missing = T
        ## ------------------------------------------
        # start with looking at the isco data, which has no missing
        test <- sum(ifelse(is.na(data[[1]]$`Level (ft)`)==T,1,0))
      
      }
    
      # unless we are looking for a specific logger
      if (is.null(specific) == T) {
        # number of data points in "isco" file
        isco_data <- data_r %>% filter(logger == "isco")
        n_isco <- nrow(isco_data)
        # if there are no missing data
        if (fill_missing == F | nrow(na.omit(isco_data)) == n_isco) {
          data_r <- data_r %>% filter(logger == "isco")
        }
      }
    }
    #browser()
    # check if weather data or rain data is needed
    if (main_type == "Rain" || any(grepl("Rainfall", what_to_plot)) == T) {
      data_rain <- read_rain(which_catch = which_catch,
                             main_p = main_path)
    }
    if (main_type == "Weather" || any(grepl("Temp", what_to_plot)) == T) {
      data_w <- read_weather(which_catch = which_catch,
                             main_p = main_path) %>%
        rename(Temperature = `Temperature °C`)
    }

    # add the flow data if plotting of flow data is required
    if (any(grepl("Flow",what_to_plot))==T) {
      data_plotting <- flow_convert(data_r, catch = which_catch,
                                    path = main_path)
    } else {
      if (exists("data_r")) data_plotting <- data_r
    }
    
    # merge with rainfall data if exists
    if (exists("data_rain")) {
        if (exists("data_plotting")) {
          # use "full_join" as timesteps are different
          data_plotting <- full_join(data_plotting,data_rain, 
                                 by = "Date and Time")
        } else data_plotting <- data_rain
    }
    # merge with weather data if exists
    if (exists("data_w")) {
      # use "full_join" as timesteps are different
      data_plotting <- full_join(data_plotting,data_w, 
                                 by = "Date and Time")
    }
    #browser()
    # manipulate data to plot exactly what is required
    
    plot_data(data_plotting, what_to_plot, time_agg = time_scale)
  # data_plotting %>%
  #   # select only relevant columns
  #   select(`Date and Time`, as.name(what_to_plot[[1]]),as.name(what_to_plot[[2]])) %>%
  #   # pivot longer for plotting in panels
  #   pivot_longer(c(as.name(what_to_plot[[1]]),as.name(what_to_plot[[2]])), values_to = "Measurements",
  #                names_to = "Variables") %>%
  #   # group everything to hourly
  #   group_by(Variables, Year = year(`Date and Time`), Month = month(`Date and Time`),
  #            Day = day(`Date and Time`), Hour = hour(`Date and Time`)) %>%
  #   summarise(Measurements = mean(Measurements, na.rm =T),
  #             Variables = unique(Variables)) %>%
  #   # add date and time back in
  #   mutate(`Date and Time` = ymd_h(paste0(Year,"-",Month,"-",Day," ",Hour))) %>%
  #   ggplot(aes(`Date and Time`,Measurements)) + geom_line() +
  #   theme_bw() + facet_wrap(~Variables, scales="free")

}

# # testing Use case 1
# read_data(main_type,which_catch,
#           what_to_plot)
# # testing Use case 2
# read_data(main_type,which_catch,
#            what_to_plot, specific = specific_in)


# Auxiliary functions
# read_flume, the output of this function is a list with data from all loggers associated with the flumes
read_flume <- function(which_catch,
                            main_p = main_path, ...) {
  # catch extra arguments in a list
  extra_args <- list(...)
  
  # extract the list of directories
  dir_list <- dir_fun(which_catch, main_p)
  #logger_order <- dir_list[[2]]
  #dir_list <- dir_list[[1]]
  #browser()
  # for a specific logger, cut the logger_order vector to just that logger
  if (is.null(extra_args[[names(extra_args) == "specific"]])==FALSE) {
      logger_order <- dir_list[[2]][grepl(extra_args[[names(extra_args) == "specific"]],
                                         dir_list[[2]], ignore.case = T)]
  } else logger_order <- dir_list[[2]]
  # run down logger order to read in the data from each logger
  data <- lapply(logger_order, read_logger_order, list(path = dir_list[[3]], dir_list = dir_list[[1]]))
  
  names(data) <- logger_order
  #browser()
  # convert the isco logger data from ft to depth in meters
  is <- grep("isco",logger_order)
  if (length(is) > 0) {
    data[[is]] <- isco_convert(data[[is]])
  }
  
  # Convert the stevens logger from volt to depth
  st <- grep("stevens",logger_order)
  if (length(st) > 0) {
    data[[st]] <- Stevens_convert(data[[st]], which_catch)
  }
 
  # return list of logger data
  return(data)
}


# # test
# data_r <- do.call(read_flume,
#                   list(which_catch = 1, 
#                        main_p = "SampleFiles"))
# str(data_r)

dir_fun <- function(w_c, m_p) {
  # check logger_order for catchments
  if (w_c< 3) {
    path <- paste(m_p,"Flumes/V1V2", sep="/")
    logger_order <- c("isco","stevens","hobou20")
    
  } else {
    path <- paste(m_p,"Flumes/V3V4", sep="/")
    logger_order <- c("isco","hobou20","stevens")
  }
  #browser()
  dir_list <- dir(path = path)
  # exclude the emergency spill way HoboU20. Need to check with Chip
  dir_list <- dir_list[!grepl("Emergency",dir_list)]
  
  return(list(dir_list,logger_order,path))  
}


# conversion of isco_logger
isco_convert <- function(data_in) {
  data_out <- data_in %>%
    mutate(`Water Level, meters` = `Level (ft)`*0.3048) %>%
    select(`Date and Time`, `Water Level, meters`, logger)
  
  return(data_out)
}


# conversion of Stevens logger
Stevens_convert <- function(data_in, which_catch) {
    data_out <- data_in %>% 
      mutate(`Water Level, meters` =
             case_when(
               which_catch == 1 ~ -0.03549 + 1.2*`Volt, V`,
               which_catch == 2 ~ -0.666 + 1.2*`Volt, V`,
               which_catch == 3 ~ 3.266 -1.28761*`Volt, V`,
               which_catch == 2 ~ -0.65 + 1.2*`Volt, V`
             )) %>%
    select(`Date and Time`,`Water Level, meters`, logger)
  return(data_out)
}


# read_logger_order function
read_logger_order <- function(logger_order_pos, ...) {
# this for loop needs to be rewritten as a function and using map
  #browser()
  extra_args <- (...)
  path <- extra_args[[grep("path",names(extra_args))]]
  dir_list <- extra_args[[grep("dir_list",names(extra_args))]]
  #for (i in 1:length(logger_order)){
  # find the path to the specific logger output
  read_path <- paste(path,dir_list[grep(logger_order_pos,dir_list,
                                        ignore.case = T)],sep="/")
  # find the list of files
  filelist <- list.files(path=read_path,pattern ="csv")
  # use do.call to call the different functions
  # needs to be extended to read files with multiple dates
  data_out <- do.call(paste0("read_",logger_order_pos),
                       # figure out which file to read
                       list(filename =filelist[ifelse(which_catch < 3,
                                                      which_catch, which_catch - 2)], 
                            input_dir = read_path)) %>% 
    # add a column with the name of logger
    mutate(logger = logger_order_pos)

    return(data_out)
}
  
# flow conversion for flumes
# HL flumes is simply an equation
HL_convert <- function(H) {
  # using http://www2.alterra.wur.nl/Internet/webdocs/ilri-publicaties/publicaties/Pub20/pub20-h7.2.pdf
  flow <- exp(0.3160 + 2.3466*log(H) + 0.2794*log(H)^2)
  return(flow)
}
# But what to do with the emergency spillway?

Tri_flume_convert <- function(data_H, catch_in, 
                              main_p) {
  # need to read in the velocity
  # find the list of files
  #browser()
  filelist <- list.files(path=paste0(main_p,"/Flumes/V3V4/ISCOsampler"),
                         pattern =".vel")
  data_v <- read_isco_velocity(filename =filelist[catch_in-2], 
                            input_dir = paste0(main_p,"/Flumes/V3V4/ISCOsampler")) 
  
  data_all <- left_join(data_H,data_v)
  # now we need to know the width of the flume
  width = 3*0.3048 # convert to m
  flow <- width*data_all$`Water Level, meters`*(data_all$`velocity (ft/s)`*0.3048)
  return(flow)
}

flow_convert <- function(data_in, catch, path) {
   # convert level in meters to m3/sec
  # HL flume
  #browser()
  if (which_catch < 3) {
    data_in <- data_in %>% 
      mutate(`Flow (m3/sec)` = HL_convert(`Water Level, meters`))
  } else {
    data_in <- data_in %>%
      mutate(`Flow (m3/sec)` = Tri_flume_convert(data_in, catch_in = catch, 
                                                             main_p = path)) 
  }
}

# read the groundwater wells
read_well <- function(which_catch,
                       fill_missing = F, main_p = main_path, Automatic = T) {
    read_path <- paste(main_p,"Wells",
                       ifelse(Automatic ==T, "Automatic", "Manual"), sep="/")
    # the filename from the main path (assuming "automatic")
    filelist <- list.files(read_path, pattern = "csv")
    # needs to be extended to read multiple date files
    data <- read_hobo_well(filelist[which_catch],read_path)
  

  if (fill_missing == T) {
    ## -----------------------------------------
    ## in here do something with fill_missing = T
    ## ------------------------------------------
  }
  # return list of logger data
  return(data)
}


read_rain <- function(which_catch,
                      fill_missing = F, main_p = main_path, Automatic = T) {
  # account for missing rain gauges for 2 and 3
  if (which_catch == 2) {
    which_catch <- 1
    message("using raingauge from catchment 1")
  }
  if (which_catch == 3) {
    which_catch <- 4
    message("using raingauge from catchment 4")
  }
  #browser()
  read_path <- paste(main_p,"Rain",
                     ifelse(Automatic ==T, "Automatic", "Manual"), sep="/")
  # the filename from the main path (assuming "automatic")
  filelist <- list.files(read_path, pattern = "csv")
  data <- read_hobo_rain(filelist[ifelse(which_catch==1,1,2)],read_path)
  
  
  if (fill_missing == T) {
    ## -----------------------------------------
    ## in here do something with fill_missing = T
    ## ------------------------------------------
  }
  # return list of logger data
  return(data)
}

# read_weather auxillary function
read_weather <- function(which_catch,
                      fill_missing = F, main_p = main_path, Automatic = T) {
  #browser()
  read_path <- paste(main_p,"Weather",sep="/")
  # the filename from the main path (assuming "automatic")
  file <- list.files(read_path, pattern = "dat")
  # this needs to be extended for multiple files and combining
  data <- read_hobo_weather(file,read_path)
  
  
  if (fill_missing == T) {
    ## -----------------------------------------
    ## in here do something with fill_missing = T
    ## ------------------------------------------
  }
  # return list of logger data
  return(data)
}

# # function to select correct row from look-up table
# rows_select <- function(what_to_plot, lookup) {
#   # find rows for first variable
#   funs_rows <- grep(what_to_plot[[1]],lookup$`variable 1`,
#                     ignore.case = T)
#   # if there is only one variable to plot
#   #(make this so it can be time later)
#   if (is.null(what_to_plot[[2]])) {
#     funs_rows_select <- lookup[is.na(lookup[funs_rows,"variable 2"]),]
#   } else {
#     funs_rows_select <- lookup[grep(what_to_plot[[2]],
#                                     lookup[funs_rows,"variable 2"],
#                                     ignore.case = T),]
#   }
#   return(funs_rows_select)
# }



