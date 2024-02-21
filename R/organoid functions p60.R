# Making functions for the organoid spike counts files.
library(tidyverse)
#' read_org_data
#'
#' Function to load organoid spikes data, that I have produced using Julia/Jupyter notebook.
#' See github.com/chloemhall/HD-MEA-Organoids
#'
#' @param spikes_file is the .txt file that has the number of spikes measured per channel (row) as a vector.
#' @param channels_file is the raw file that has the channel numbers used in this analysis. Only relevant when analysis was performed on a subset of channels.
#' @param condition is the condition of the recording ie. in the organoid experiments, Control, PTX or Propofol.
#' @return returns useful index of row.column
#' @export

read_org_data <- function(spikes_file, channels_file, condition) {
  # Load data
  org_data <- read.table(spikes_file, header = FALSE, sep = "\t")
  organoid_channels <- read.table(channels_file, header = TRUE, sep = "\t")

  # Combine data frames
  org_data <- cbind(org_data, organoid_channels)

  # Rename columns
  names(org_data)[1] <- "Num.Spikes"
  names(org_data)[2] <- "Channels"
  org_data$Condition <- condition

  # Calculate frequency
  time = 292.775 #for original 300s at 19,754 Sampling rate
  org_data$Frequency <- org_data$Num.Spikes / time
  return(org_data)
}

process_organoid_data <- function(spikes_file, channels_file, condition) {
  # Load data
  org_data <- read.table(spikes_file, header = FALSE, sep = "\t")
  organoid_channels <- read.table(channels_file, header = TRUE, sep = "\t")

  # Combine data frames
  org_data <- cbind(org_data, organoid_channels)

  # Rename columns
  names(org_data)[1] <- "Num.Spikes"
  names(org_data)[2] <- "Channels"
  org_data$Condition <- condition

  # Calculate frequency
  time = 292.775 #taken from the matrix size / SamplingRate, calculated in Julia.
  org_data$Frequency <- org_data$Num.Spikes / time

  # Remove specific dodgy channel
  org_data <- org_data %>%
    filter(Channels != '3849') #this channel is always saturated, regardless of organoid & drug.

  # Filter for top 50
  top_50 <- head(org_data[order(-org_data$Num.Spikes), ], 50)

  return(top_50)
}
process_organoid_data_top20 <- function(spikes_file, channels_file, condition) {
  # Load data
  org_data <- read.table(spikes_file, header = FALSE, sep = "\t")
  organoid_channels <- read.table(channels_file, header = TRUE, sep = "\t")

  # Combine data frames
  org_data <- cbind(org_data, organoid_channels)

  # Rename columns
  names(org_data)[1] <- "Num.Spikes"
  names(org_data)[2] <- "Channels"
  org_data$Condition <- condition

  # Calculate frequency
  time = 292.775 #taken from the matrix size / SamplingRate, calculated in Julia.
  org_data$Frequency <- org_data$Num.Spikes / time

  # Remove specific dodgy channel
  org_data <- org_data %>%
    filter(Channels != '3849') #this channel is always saturated, regardless of organoid & drug.

  # Filter for top 20
  top_20 <- head(org_data[order(-org_data$Num.Spikes), ], 20)

  return(top_20)
}
graph_org_top50 <- function (combined_top50, title) {

  #make channel a cat variable
  combined_top50$Channels <- as.factor(combined_top50$Channels)

  #graph
  graph1 <- ggplot(combined_top50, aes(x= Channels, y= Frequency, fill= Condition))+
    geom_col(alpha=0.9, position = "dodge")+
    scale_fill_manual(values = c("#333333", "#9933CC", "#66CC66")) +
    ylim(0, 3.0)+
    theme_bw()+
    labs(title = title, y = "Frequency (Hz)")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 5))+
    theme(plot.title = element_text(hjust = 0.5))

  return(graph1)
}
graph_org_top20 <- function (combined_top20, title) {

  #make channel a cat variable
  combined_top20$Channels <- as.factor(combined_top20$Channels)

  #graph
  graph20 <- ggplot(combined_top20, aes(x= Channels, y= Frequency, fill= Condition))+
    geom_col(alpha=0.9, position = "dodge")+
    scale_fill_manual(values = c("#333333", "#9933CC", "#66CC66")) +
    ylim(0, 3.0)+
    theme_bw()+
    labs(title = title, y = "Frequency (Hz)")+
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 5))+
    theme(plot.title = element_text(hjust = 0.5))

  return(graph20)
}
#' make_raster_plot
#'
#' Uses the timestamps to make a rasterplot of the spikes.
#' @param Condition is the condition of recording e.g. Control, PTX, Propofol. This also corresponds to the colours of the plots.
#' @param title is the title you would like to appear on the plot "Title1"
#' @param df is of course the df with the timestamps you want to plot. This is returned from the extract_timestamps function.
#' @export
make_raster_plot<- function (df, title, Condition) {

  conditionColour <- c("Control" = "#333333", "Propofol" = "#9933CC", "PTX" = "#66CC66")
  #graph
  rasterplot <- ggplot(data=df, aes(x = Real.Time, y=Channel, colour =Condition))+
    geom_point(size= 0.01)+
    scale_color_manual(values = conditionColour) +
    xlim(0.0, 300.0)+
    theme_classic()+
    theme(axis.text.y = element_text(size = 4))+
    labs(title = title, x = "Time (s)") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")

  return(rasterplot)
}

find_active_channels <- function(org_data) {
  active_channels <- org_data %>%
    filter(Frequency > 0.5)

  active_channels_by_condition <- active_channels %>%
    group_by(Condition, Organoids) %>%
    summarise(Active.Channels = n())
  # Remove specific dodgy channel
  active_channels_by_condition <- active_channels_by_condition %>%
    filter(Active.Channels != '3849') #this channel is always saturated, regardless of organoid & drug.

  return(active_channels_by_condition)
}

channel_to_index <- function(Ch){
  max.full.rows <- floor(Ch / 64 )
  col.idx <- Ch - (max.full.rows*64)
  row.idx <- (max.full.rows+1)
  Ch.idx <- c(row.idx, col.idx)
  return(Ch.idx)
}
#' extract_timestamps
#'
#' Turns the data points of spikes timed into seconds.
#' Note that this is for a 19254 effective Sampling Rate, ie. actual recorded SR was 19754 but after timestamp deletion the eff.SR is 19254.
#' It is important also to check that you have the right number of seconds per chunk, created in Julia, in this case 5 seconds per voltage chunk.
#' @param dataframe is the previously imported file usually saved as format "Timestamps_of_spikes_60.txt"
#'@export
extract_timestamps <- function(dataframe) {
  #trial stack overflow LMc #### THIS WORKS!!!!!!!
  eff.SR <- 19254
  eff.SR.per.seg <- eff.SR*5
  useful_timestamp_data<- dataframe|>
    mutate(across(everything(), \(x) imap(str_extract_all(x, "\\d+"), ~ as.numeric(.x) + (.y-1)*eff.SR.per.seg))) |>
    pivot_longer(everything(), cols_vary = "slowest", values_to = "Time", names_to = "Channel") |>
    unnest_longer(Time)

  return(useful_timestamp_data)

}
#' extract_timestamps
#'
#' Turns the data points of spikes timed into seconds.
#' Note that this is for a 19254 effective Sampling Rate, ie. actual recorded SR was 19754 but after timestamp deletion the eff.SR is 19254.
#' It is important also to check that you have the right number of seconds per chunk, created in Julia, in this case 3 seconds per voltage chunk.
#' @param dataframe is the previously imported file usually saved as format "Timestamps_of_spikes_60.txt"
#'@export
extract_timestamps_altered <- function(dataframe) {
  #trial stack overflow LMc #### THIS WORKS!!!!!!!
  eff.SR <- 19254
  eff.SR.per.seg <- eff.SR*3
  useful_timestamp_data<- dataframe|>
    mutate(across(everything(), \(x) imap(str_extract_all(x, "\\d+"), ~ as.numeric(.x) + (.y-1)*eff.SR.per.seg))) |>
    pivot_longer(everything(), cols_vary = "slowest", values_to = "Time", names_to = "Channel") |>
    unnest_longer(Time)

  return(useful_timestamp_data)

}
#input active_channels_filter( df_tofilter, giant_data, "organoid", "condition")
active_channels_filter <- function(df1, giant_data, organoid, condition) {
  org <- giant_data[giant_data$Organoids == organoid, ]
  org_active <- org[org$Frequency >= 0.5, ]
  org_active_control <- org_active[org_active$Condition == condition, ]
  channel_to_exclude ="3849" #this channel is saturated and dodgy across the whole day.
  # Extract the column names from org_active_control$Channels excluding a specific channel
  columns_to_keep <- setdiff(as.character(org_active_control$Channels), channel_to_exclude)
  # Use select to filter df1 columns based on columns_to_keep
  filtered_active_df <- select(df1, all_of(columns_to_keep))

  return(filtered_active_df)

}
#' save_graph
#'
#' Saves the graph as a pdf plot 6 inches wide and 4 inches high. Saves in a specified graphs directory, and then returns to the working directory.
#' @param graph is the variable where the desired graph to save is stored.
#' @param filename is the name you would like to give the saved graph file e.g. "graph1"
#' @export
#input save_graph(graph, "raster_plot_control.pdf")
save_graph <- function(graph, filename){
  current_directory <- getwd()
  setwd("/Users/chloe/Documents/R/Organoids Results/Organoid Results P60/Graphs")
  pdf(file = filename ,
      width = 6, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  print(graph)
  dev.off()
  setwd(current_directory)
}
#test function for burst detection#
#to create...
