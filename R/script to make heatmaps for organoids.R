library("devtools")
library("roxygen2")
library(tidyverse)
library("HDMEATools")
library(viridis)
load_all();

#Step 1 #
#load data
giant.df <- read.csv("/Users/chloe/Documents/R/Organoids Results/all.data.orgs.csv")

#workflow tidy below to run all these functions one by one
test4<- subtract_frequency(giant.df, "P60", "3", "PTX", "Control") # function1
test5<- channel_to_index2(test4) #function2

#need to delete dodgy channel 3849 lol...
# Assuming your dataframe is named test5
test5 <- test5[test5$Channels != 3849, ]


#test6<- plot_the_difference(test5) #function3

#using ggplot2:: geom_tile()
graph1<-ggplot(test5, aes(x=col.idx, y=row.idx, fill= Frequency_Difference)) +
  geom_tile()+
  ggtitle("P60 Organoid 3 PTX - Control")+
  scale_fill_viridis(discrete=FALSE, limits = c(-0.5, 1.0)) +
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5))+
  coord_cartesian(xlim = c(0, 64), ylim = c(0, 64))

graph1

#save_graph(graph1, "P90 Organoid 2 differences in PTX - Ctrl.pdf")

channel_to_index2 <- function(df1) {
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

