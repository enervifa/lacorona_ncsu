# Source the read scripts
script_dir <- "LaCorona_Scripts/lacorona"
source(paste(script_dir,"read_files.R",sep="/"))
source(paste(script_dir,"read_rain.R",sep="/"))
source(paste(script_dir,"read_weather.R",sep="/"))
source(paste(script_dir,"read_well.R",sep="/"))
source(paste(script_dir,"generic_read.R",sep="/"))
source(paste(script_dir,"data_plotting.R",sep="/"))

### ----- use case 1
# # What do you want to read?
# main_type <- "Flumes"
# which_catch <- 4 # or 2, 3 4
# what_to_plot <- c("Water Level, meters","Flow (m3/sec)")

## ----- use case 1a
main_type <- "Flumes"
which_catch <- 4 # or 2, 3 4
what_to_plot <- c("Water Level, meters","Flow (m3/sec)")
time_for_plot <- "day" # "hour" or "month"

# ## ---- use case 2
# main_type <- "Flumes"
# which_catch <- 1 # or 2, 3 4
# what_to_plot <- list("Water Level, meters","Temperature")
# specific_in <- "HOBOU20"
# # or specific can be ISCO, HOBOU20, HOBOU12(Stevens), Emergency (only for V1V2)

# ## --- use case 3
# main_type <- "Rain"
# which_catch <- 2 # or 1, 3 4
# what_to_plot <- list("Rain, mm","Temperature")
# # there is also rainfall and Temp, °C
# #what_to_plot <- list("rainfall","Temp, °C")

# # testing Use case 1 & 3
# read_data(main_type,which_catch,
#           what_to_plot)
# # testing Use case 2
# read_data(main_type,which_catch,
#           what_to_plot, specific = specific_in)

# testing Use case 1a
read_data(main_type,which_catch,
          what_to_plot, time_scale = time_for_plot)
