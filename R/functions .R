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
#' Converts a simple integer of channel into more useful index
#' ( on 3Brain Accura chips, this is 64 rows x 64 columns. eg. channel 2 is row 1, col 2 ie. 1.2 )
#'
#' @param Ch is an integer 1:4096 of the channel / electrode.
#' @return returns useful index of row.column
#' @examples
#' > channel<- channel_to_index(65)
#' > print(channel)
#' > 2 1 #ie. row 2. column 1
#' @export

channel_to_index <- function(Ch){
  if (Ch %% 64 == 0){
    row.idx <- Ch/64
    col.idx <- 64
  }else{
    max.full.rows <- floor(Ch / 64 )
    row.idx <- (max.full.rows+1)
    col.idx <- Ch - (max.full.rows*64) }

  Ch.idx <- c(row.idx, col.idx)
  return(Ch.idx)
}

#' subtract_frequency
#'
#'  take channels from the channelsdf and extract those channels from targetdf
#' @param channelsdf is a df that has the channels you want / are interested in
#' @param targetdf is a df with many channels and you want to search it & find the ones you want, which are in channelsdf
#' @return gives us end_df which is a df where the frequency of x has been subtracted from y.
#' @export
subtract_frequency <- function(targetdf, channelsdf){
  #take channels from the channelsdf and extract those channels from targetdf
  # ie in this case channelsdf has the channels you want.
  holding_df <- targetdf %>% filter(Channels %in% c(channelsdf$Channels))
  h2 <- holding_df %>% filter(Condition == "PTX") %>% arrange(Channels)

  h3 <- holding_df %>%  filter(Condition =="Control")%>% arrange(Channels)

  end_df <- inner_join(h2, h3, by = "Channels") %>%
    mutate(Frequency_Difference = Frequency.x - Frequency.y) %>%
    select(Channels, Frequency_Difference)
  #  end_df$Ch.Idx <- channels_to_index(ptx_ctrl$Channels)
  #  colnames(end_df)[3] <- "Row.idx"
  #   colnames(end_df)[4] <- "Col.idx"

  return(end_df)
}


