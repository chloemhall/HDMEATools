find_top50_channels <- function(df, age, organoid, condition){
  df1 <- df %>%
    filter(Condition %in% c(condition)) %>%
    filter(Age %in% c(age)) %>%
    filter(Organoids %in% c(organoid))

  df2 <-  head(df1[order(-df1$Num.Spikes), ], 50)     # Filter for top 50

  return(df2) #This function subsets the data df to just find the top 50
  #channels from the entered situation.
}


