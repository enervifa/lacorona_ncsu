# function to plot data in generic_read
plot_data <- function(df, what_to_plot, time_agg = "hour",
                      pop_up = T) {
  df1 <- df %>%
    # select only relevant columns
    select(`Date and Time`, as.name(what_to_plot[[1]]),
           as.name(what_to_plot[[2]])) %>%
    # pivot longer for plotting in panels
    pivot_longer(c(as.name(what_to_plot[[1]]),as.name(what_to_plot[[2]])), values_to = "Measurements",
                 names_to = "Variables") 
  
  if (time_agg == "hour") {
    plot_df <- df1 %>%
      # group everything to hourly
      group_by(Variables, Year = year(`Date and Time`), Month = month(`Date and Time`),
               Day = day(`Date and Time`), Hour = hour(`Date and Time`)) %>%
      summarise(Measurements = mean(Measurements, na.rm =T),
                Variables = unique(Variables)) %>%
      # add date and time back in
      mutate(`Date and Time` = ymd_h(paste0(Year,"-",Month,"-",Day," ",Hour)))
  }
  if (time_agg == "day") {
    plot_df <- df1 %>%
      # group everything to daily
      group_by(Variables, Year = year(`Date and Time`), Month = month(`Date and Time`),
               Day = day(`Date and Time`)) %>%
      summarise(Measurements = mean(Measurements, na.rm =T),
                Variables = unique(Variables)) %>%
      # add date and time back in
      mutate(`Date and Time` = ymd(paste0(Year,"-",Month,"-",Day)))
  }

  if(time_agg == "month") {
    plot_df <- df1 %>%
      # group everything to monthly
      group_by(Variables, Year = year(`Date and Time`), Month = month(`Date and Time`)) %>%
      summarise(Measurements = mean(Measurements, na.rm =T),
                Variables = unique(Variables)) %>%
      # add date and time back in
      mutate(`Date and Time` = ymd(paste0(Year,"-",Month,"-01")))
  } 
  if (pop_up == T) X11()
  
  # make the plot
  plot_df %>%
      ggplot(aes(`Date and Time`,Measurements)) + geom_line() +
    theme_bw() + facet_wrap(~Variables, scales="free")
  
}

