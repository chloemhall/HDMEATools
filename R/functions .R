# functions for organoid HD mea analysis

#' find_top50_channels
#'
#' Ranks the channels in a df by frequency, and returns the top 50 in a new df.
#' @param C_temp The temperature in degrees Celsius
#' @param  df is the dataframe to be filtered. In my case, I have one giant df, so I have other factors to filter by.
#' @param age is the desired age of the organoid of interested.
#' @param organoid is the specific organoid I am interested in
#' @param condition is either Control, PTX or Propofol i.e. drug condition.
#' @return Has filtered the original df to only keep the top 50 most active channels.
#' @examples
#' org2_p60_top50_control <- find_top50_channels(giant.df, "P60", "2", "Control")
#' @export
find_top50_channels <- function(df, age, organoid, condition){
  df1 <- df %>%
    filter(Condition %in% c(condition)) %>%
    filter(Age %in% c(age)) %>%
    filter(Organoids %in% c(organoid))

  df2 <-  head(df1[order(-df1$Num.Spikes), ], 50)     # Filter for top 50

  return(df2) #This function subsets the data df to just find the top 50
  #channels from the entered situation.
}
#' channel_to_index
#'
#' Converts a simple integer of channel into more useful index, for a list of channels in df1.
#' ( on 3Brain Accura chips, this is 64 rows x 64 columns. eg. channel 2 is row 1, col 2 ie. 1.2 )
#'
#'@param df1 is the table with the list of integers "Channels" of the electrodes.
#' @return returns new df with a useful index of rows and columns, listed in separate columns.
#' @examples
#' > new.df<- channel_to_index(test.dataframe)
#' > print(new.df)
#' >
#' @export
channel_to_index <- function(df1) {
  # Make a copy of the dataframe
  df.new <- df1

  # Iterate through each row of the dataframe
  for (i in 1:nrow(df1)) {
    # Extract the channel number from each row
    Ch <- df1$Channels[i]

    # Calculate row and column indices
    row.idx <- ceiling(Ch / 64)  # Calculate the row index
    col.idx <- Ch %% 64          # Calculate the column index

    # If col.idx is zero, set it to 64
    if (col.idx == 0) {
      col.idx <- 64
    }

    # Assign row and column indices to the new dataframe
    df.new[i, "row.idx"] <- row.idx
    df.new[i, "col.idx"] <- col.idx
  }

  return(df.new)
}

#' subtract_frequency
#'
#'  take channels from the giant dataframe and extracts the channels of interest, performing a subtraction calculation of frequency per channel
#' @param all.data is a df that has all the data in one giant table. You will search and filter it.
#' @param  age Age of the organoid / experiment you want to select.
#' @param organoid The repeat number of the organoid / experiment you want to select.
#' @param condition1 This is the drug condition you want to subtract from ie. baseline.
#' @param condtion2 This is the drug condition you want to subtract from condition1
#' @return gives us t3_df which is a df where the frequency of condition2 has been subtracted from condition1.
#' @export
  subtract_frequency <- function(all.data, age, organoid, condition1, condition2){
    #search the giant df. for the right conditions etc.
    condition1.df <- all.data %>% filter(Condition == condition1, Organoids == organoid, Age == age)
    condition2.df <- all.data %>% filter(Condition == condition2, Organoids == organoid, Age == age)
    #now subtract the frequency of condition2.df from condition1.df per channel.
    t3_df <- inner_join(condition1.df, condition2.df, by = "Channels") %>%
      mutate(Frequency_Difference = Frequency.x - Frequency.y) %>%
      select(Channels, Frequency_Difference)
    return(t3_df)
  }
  #  end_df$Ch.Idx <- channels_to_index(ptx_ctrl$Channels)
  #  colnames(end_df)[3] <- "Row.idx"
  #   colnames(end_df)[4] <- "Col.idx"

#' plot_the_difference
#'
#' Make a heatmap of the difference in activity between two conditions on the HD-MEA.
#' @param df.new is the df after subtract_frequency and channel_to_index
#' @return a matrix representing the difference in activity per electrode
#' @export
plot_the_difference <- function(df.new) {
  # Make an empty (zeroes) 64 x 64 matrix i.e., blank HD-MEA
  grid_matrix <- matrix(0, nrow = 64, ncol = 64)

loop.length <- length(df.new$Channels) #length of loop
# Loop through each row of the data frame
  for (i in 1:loop.length) {
    row_idx <- df.new$row.idx[i]
    col_idx <- df.new$col.idx[i]
    # Set the corresponding cell to the value of freq_difference
    grid_matrix[row_idx, col_idx] <- df.new$Frequency_Difference[i]
  }
  return(grid_matrix)
}


#' read_org_data
#'
#' Function to load organoid spikes data, that I have produced using Julia/Jupyter notebook.
#' See github.com/chloemhall/HD-MEA-Organoids
#'
#' @param spikes_file is the .txt file that has the number of spikes measured per channel (row) as a vector.
#' @param channels_file is the raw file that has the channel numbers used in this analysis. Only relevant when analysis was performed on a subset of channels.
#' @param condition is the condition of the recording ie. in the organoid experiments, Control, PTX or Propofol.
#' @return returns a dataframe with all the data re: spikes and Frequency for the relevant organoid and condition.
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

#' process_organoid_data
#'
#' Function does the same as read_org_data but returns just the top 50 most active channels.
#' @param spikes_file is the .txt file that has the number of spikes measured per channel (row) as a vector.
#' @param channels_file is the raw file that has the channel numbers used in this analysis. Only relevant when analysis was performed on a subset of channels.
#' @param condition is the condition of the recording ie. in the organoid experiments, Control, PTX or Propofol.
#' @return returns a df with just the top 50 active electrodes
#' @export
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

#' find_active_channels
#'
#'sorts the data frame to find all values with a mean Frequency of >0.5Hz.
#' @param org_data
#' @param dodgy_channels input any noticeably dodgy channels, e.g. that always seem high in frequency regardless of conditions.
#' @return returns df of the channels with mean frequency over 0.5Hz.
#' @export
find_active_channels <- function(org_data, dodgy_channels) {
  active_channels <- org_data %>%
    filter(Frequency > 0.5)

  active_channels_by_condition <- active_channels %>%
    group_by(Condition, Organoids) %>%
    summarise(Active.Channels = n())
  # Remove specific dodgy channel

  active_channels_by_condition <- active_channels_by_condition %>%
    filter(Active.Channels != dodgy_channels) #this channel is always saturated, regardless of organoid & drug.
#check that this works now! filter(Active.Channels != '3849') was the original.
  return(active_channels_by_condition)
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
