library("devtools")
library("roxygen2")
library(tidyverse)
library("HDMEATools")
library(viridis)
#load_all();

#Step 1 #
#load data
giant.df <- read.csv("/Users/chloe/Documents/R/Organoids Results/all.data.orgs.csv")

#workflow tidy below to run all these functions one by one
test4<- subtract_frequency(giant.df, "P90", "four", "PTX", "Control") # function1
test5<- channel_to_index(test4) #function2

#need to delete dodgy channel 3849 lol...
# Assuming your dataframe is named test5
test5 <- test5[test5$Channels != 3849, ]


test6<- plot_the_difference(test5) #function3

#using ggplot2:: geom_tile()
graph1<-ggplot(test5, aes(x=col.idx, y=row.idx, fill= Frequency_Difference)) +
  geom_tile()+
  ggtitle("P90 Organoid 4 PTX - Control")+
  scale_fill_viridis(discrete=FALSE) +
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5))

graph1

save_graph(graph1, "P90 Organoid 4 differences in PTX - Ctrl ")


