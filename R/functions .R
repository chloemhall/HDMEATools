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
  list_of_channels <- df1$Channels
  df.new <- df1
  for (i in 1:length(list_of_channels)) {
    Ch <- list_of_channels[i]
    if (Ch %% 64 == 0) {
      row.idx <- Ch / 64
      col.idx <- 64
    } else {
      max.full.rows <- floor(Ch / 64 )
      row.idx <- (max.full.rows + 1)
      col.idx <- Ch - (max.full.rows * 64)
    }
    df.new[i, "row.idx"] <- row.idx
    df.new[i, "col.idx"] <- col.idx
  }
  return(df.new)
}
#' subtract_frequency
#'
#'  take channels from the channelsdf and extract those channels from targetdf
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
    t3_df <- inner_join(t1.df, t2.df, by = "Channels") %>%
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

  # Loop through each row of the data frame
  for (i in 1:nrow(df.new)) {
    row_idx <- df.new$row.idx[i]
    col_idx <- df.new$col.idx[i]
    # Set the corresponding cell to the value of freq_difference
    grid_matrix[row_idx, col_idx] <- df.new$Frequency_Difference[i]
  }
  return(grid_matrix)
}


