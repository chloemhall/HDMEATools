#look at just propofol / ptx active channels
#load data: giant data perhaps ?
#setwd("/Users/chloe/Documents/R/Organoids Results")
library(ggplot2)
library(dplyr)
# begin here.
giant.organoid.data <- read.csv(file.choose(), header=T) # navigate to file all.data.org.csv

# now find the top 50 channels from the control conditions.
##### FUNCTION #####
find_top50_channels <- function(df, age, organoid, condition){
  df1 <- df %>%
    filter(Condition %in% c(condition)) %>%
    filter(Age %in% c(age)) %>%
    filter(Organoids %in% c(organoid))

  df2 <-  head(df1[order(-df1$Num.Spikes), ], 50)     # Filter for top 50

  return(df2) #This function subsets the data df to just find the top 50
  #channels from the entered situation.
}
##### P90 ####
#organoid 4
o4_p90_control <- find_top50_channels(giant.organoid.data, "P90", "four", "Control")
o4_p90_prop <- find_top50_channels(giant.organoid.data, "P90", "four", "Propofol")
o4_p90_ptx <- find_top50_channels(giant.organoid.data, "P90", "four", "PTX")
#now I want to find just the intersections
# var1: active in propofol not control
unique_channels_in_ptx <- setdiff(o4_p90_ptx$Channels, o4_p90_control$Channels)
#subset the datasets based in only active in prop not control...
#select the channels from control that were unique to prop
# Filter rows from df1 where Channels is in unique_channels_in_df2
#I want now to go back to giant.organoid.data and now grab the channel from control for that.
org4_p90 <- giant.organoid.data %>%
  filter(Age %in% c("P90")) %>%
  filter(Organoids %in% c("four"))

org4_p90_lowPTX <- org4_p90 %>% filter(Channels %in% unique_channels_in_prop)
#tidy up, remove old non-needed vars.
rm(o4_p90_control,o4_p90_prop,o4_p90_ptx,org4_p90)

# organoid 3 p90
o3_p90_control <- find_top50_channels(giant.organoid.data, "P90", "three", "Control")
o3_p90_propofol <- find_top50_channels(giant.organoid.data, "P90", "three", "Propofol")
o3_p90_ptx <- find_top50_channels(giant.organoid.data, "P90", "three", "PTX")
# This list finds unique channels.
UC.o3.p90 <- setdiff(o3_p90_ptx$Channels, o3_p90_control$Channels)
org3_p90 <- giant.organoid.data %>%
  filter(Age %in% c("P90")) %>%
  filter(Organoids %in% c("three"))

org3_p90_lowPTX <- org3_p90 %>% filter(Channels %in% UC.o3.p90)

# maybe for ease, we remove all the non-needed dfs after calculting the final.
rm(o3_p90_control,o3_p90_propofol,o3_p90_ptx)
rm(org3_p90)

# organoid 2 p90
o2_p90_control <- find_top50_channels(giant.organoid.data, "P90", "two", "Control")
o2_p90_propofol <- find_top50_channels(giant.organoid.data, "P90", "two", "Propofol")
o2_p90_ptx <- find_top50_channels(giant.organoid.data, "P90", "two", "PTX")
# This list finds unique channels.
uc.o2.p90 <- setdiff(o2_p90_ptx$Channels, o2_p90_control$Channels)
org2_p90 <- giant.organoid.data %>%
  filter(Age %in% c("P90")) %>%
  filter(Organoids %in% c("two"))

org2_p90_lowPTX <- org2_p90 %>% filter(Channels %in% uc.o2.p90)

# maybe for ease, we remove all the non-needed dfs after calculating the final.
rm(o2_p90_control,o2_p90_propofol,o2_p90_ptx)
rm(org2_p90)
#think maybe we have none from this organoid!!!!!
#####---------------------------------------------------------------------------------------####
# Younger orgs.
#P60 organoid 2
o2_p60_control <- find_top50_channels(giant.organoid.data, "P60", "2", "Control")
o2_p60_propofol <- find_top50_channels(giant.organoid.data, "P60", "2", "Propofol")
o2_p60_ptx <- find_top50_channels(giant.organoid.data, "P60", "2", "PTX")
# This list finds unique channels.
uc.o2.p60 <- setdiff(o2_p60_ptx$Channels, o2_p60_control$Channels)
org2_p60 <- giant.organoid.data %>%
  filter(Age %in% c("P60")) %>%
  filter(Organoids %in% c("2"))

org2_p60_lowPTX <- org2_p60 %>% filter(Channels %in% uc.o2.p60)

# maybe for ease, we remove all the non-needed dfs after calculting the final.
rm(o2_p60_control,o2_p60_propofol,o2_p60_ptx)
rm(org2_p60)

#P60 organoid 3
o3_p60_control <- find_top50_channels(giant.organoid.data, "P60", "3", "Control")
o3_p60_propofol <- find_top50_channels(giant.organoid.data, "P60", "3", "Propofol")
o3_p60_ptx <- find_top50_channels(giant.organoid.data, "P60", "3", "PTX")
# This list finds unique channels.
uc.o3.p60 <- setdiff(o3_p60_ptx$Channels, o3_p60_control$Channels)
org3_p60 <- giant.organoid.data %>%
  filter(Age %in% c("P60")) %>%
  filter(Organoids %in% c("3"))

org3_p60_lowPTX <- org3_p60 %>% filter(Channels %in% uc.o3.p60)

# maybe for ease, we remove all the non-needed dfs after calculting the final.
rm(o3_p60_control,o3_p60_propofol,o3_p60_ptx)
rm(org3_p60)

#P60 organoid 4
o4_p60_control <- find_top50_channels(giant.organoid.data, "P60", "4", "Control")
o4_p60_propofol <- find_top50_channels(giant.organoid.data, "P60", "4", "Propofol")
o4_p60_ptx <- find_top50_channels(giant.organoid.data, "P60", "4", "PTX")
# This list finds unique channels.
uc.o4.p60 <- setdiff(o4_p60_ptx$Channels, o4_p60_control$Channels)
org4_p60 <- giant.organoid.data %>%
  filter(Age %in% c("P60")) %>%
  filter(Organoids %in% c("4"))

org4_p60_lowPTX <- org4_p60 %>% filter(Channels %in% uc.o4.p60)

# maybe for ease, we remove all the non-needed dfs after calculting the final.
rm(o4_p60_control,o4_p60_propofol,o4_p60_ptx)
rm(org4_p60)

#p60 organoid 5
o5_p60_control <- find_top50_channels(giant.organoid.data, "P60", "5", "Control")
o5_p60_propofol <- find_top50_channels(giant.organoid.data, "P60", "5", "Propofol")
o5_p60_ptx <- find_top50_channels(giant.organoid.data, "P60", "5", "PTX")
# This list finds unique channels.
uc.o5.p60 <- setdiff(o5_p60_ptx$Channels, o5_p60_control$Channels)
org5_p60 <- giant.organoid.data %>%
  filter(Age %in% c("P60")) %>%
  filter(Organoids %in% c("5"))

org5_p60_lowPTX <- org5_p60 %>% filter(Channels %in% uc.o5.p60)

# maybe for ease, we remove all the non-needed dfs after calculting the final.
rm(o5_p60_control,o5_p60_propofol,o5_p60_ptx)
rm(org5_p60)

#p60 organoid 6
o6_p60_control <- find_top50_channels(giant.organoid.data, "P60", "6", "Control")
o6_p60_propofol <- find_top50_channels(giant.organoid.data, "P60", "6", "Propofol")
o6_p60_ptx <- find_top50_channels(giant.organoid.data, "P60", "6", "PTX")
# This list finds unique channels.
uc.o6.p60 <- setdiff(o6_p60_ptx$Channels, o6_p60_control$Channels)
org6_p60 <- giant.organoid.data %>%
  filter(Age %in% c("P60")) %>%
  filter(Organoids %in% c("6"))

org6_p60_lowPTX <- org6_p60 %>% filter(Channels %in% uc.o6.p60)

# maybe for ease, we remove all the non-needed dfs after calculting the final.
rm(o6_p60_control,o6_p60_propofol,o6_p60_ptx)
rm(org6_p60)



#####--------------------------------------now have a look----------------------------------####
#make the tables into one
#dfP is a large bound data table of all the data selected from the uniquely
#active propofol channels.
dfPTX <- rbind(org2_p60_lowPTX,org3_p60_lowPTX, org4_p60_lowPTX,
             org5_p60_lowPTX, org6_p60_lowPTX,
             org2_p90_lowPTX, org3_p90_lowPTX, org4_p90_lowPTX)

graph.a <- graph_org_top50(dfP, "Channels uniquely active in PTX")+
  facet_wrap(~Age)

graph.a
save_graph(graph.a, "graphs high in PTX not control.pdf")
#so what I want to see, is that the activity in these channels goes up before and after...
#So maybe raster plots with these specific channels?  ####
#So step 1 is load the data of timestamps.
# P60 org4####
setwd("/Users/chloe/Documents/R/Organoids Results/Organoid Results P60/")
org4_control_timestamps <- read.delim("org4_control_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
org4_propofol_timestamps <- read.delim("org4_propofol_Timestamps_of_spikes_60 (2).txt", header = F, sep = "\t", dec = ".")
org4_ptx_timestamps <- read.delim("org4_ptx_Timestamps_of_spikes_60 (2).txt", header = F, sep = "\t", dec = ".")
#rename the channels.
#import the channels
org4.channels <- read.table("OrganoidChannels4.txt", header = T, sep="\t")
new_column_names <- org4.channels$X1
colnames(org4_control_timestamps) <- as.character(new_column_names)
colnames(org4_propofol_timestamps) <- as.character(new_column_names)
colnames(org4_ptx_timestamps) <- as.character(new_column_names)
#Then select just the channels I want.
#they are in org4_p60_lowPTX
channels.I.want <- org4_p60_lowPTX$Channels
#select only the channels I want from these timestamp tables
df4.control <- org4_control_timestamps[, colnames(org4_control_timestamps) %in% channels.I.want]
df4.propofol <- org4_propofol_timestamps[, colnames(org4_propofol_timestamps) %in% channels.I.want]
df4.PTX <- org4_ptx_timestamps[, colnames(org4_ptx_timestamps) %in% channels.I.want]
#what input does the extract_timestamps function need??
#then extract the timestamps...
#---------------------------------------------------------------------------------#
# clean up the messy data, extract the timestamps from the chunked file #
# and make it continuous
o4.control.TS<- extract_timestamps(df4.control)
o4.propofol.TS <- extract_timestamps(df4.propofol)
o4.ptx.TS <- extract_timestamps(df4.PTX)
#now actually use the timestamps info.
eff.SR <- 19254 #real.SR <- 19754 #sampling rate #effective.SR <- real.SR - 500
o4.control.TS$Real.Time <- o4.control.TS$Time / eff.SR # is this correct? think about this a wee bit more...
o4.propofol.TS$Real.Time <- o4.propofol.TS$Time / eff.SR
o4.ptx.TS$Real.Time <- o4.ptx.TS$Time / eff.SR
#make graph
graph.b <- make_raster_plot(o4.control.TS, "Raster Plot - P60 org4 Control: PTX-selection", "Control")
graph.b
#save_graph(graph.b, "Org4- p60 control raster plot PTX-selection .pdf")
graph.c <- make_raster_plot(o4.propofol.TS, "Raster Plot - P60 org4 Propofol: PTX-selection", "Propofol")
graph.c
#save_graph(graph.c, "Org4- p60 propofol raster plot- PTX-selection .pdf")
graph.d <- make_raster_plot(o4.ptx.TS, "Raster Plot - P60 org4 PTX: PTX-selection", "PTX")
graph.d
#save_graph(graph.d, "Org4- p60 ptx raster plot PTX-selection.pdf")

# P60 org5####
setwd("/Users/chloe/Documents/R/Organoids Results/Organoid Results P60/")
org5_control_timestamps <- read.delim("org5_control_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
org5_propofol_timestamps <- read.delim("org5_propofol_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
org5_ptx_timestamps <- read.delim("org5_ptx_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
#rename the channels.
#import the channels
org5.channels <- read.table("OrganoidChannels5.txt", header = T, sep="\t")
new_column_names <- org5.channels$X1
colnames(org5_control_timestamps) <- as.character(new_column_names)
colnames(org5_propofol_timestamps) <- as.character(new_column_names)
colnames(org5_ptx_timestamps) <- as.character(new_column_names)
#Then select just the channels I want.
#they are in org5_p60_lowPTX
channels.I.want <- org5_p60_lowPTX$Channels
#select only the channels I want from these timestamp tables
df5.control <- org5_control_timestamps[, colnames(org5_control_timestamps) %in% channels.I.want]
df5.propofol <- org5_propofol_timestamps[, colnames(org5_propofol_timestamps) %in% channels.I.want]
df5.PTX <- org5_ptx_timestamps[, colnames(org5_ptx_timestamps) %in% channels.I.want]
#what input does the extract_timestamps function need??
#then extract the timestamps...
#---------------------------------------------------------------------------------#
# clean up the messy data, extract the timestamps from the chunked file #
# and make it continuous
org5.control.TS<- extract_timestamps(df5.control)
org5.propofol.TS <- extract_timestamps(df5.propofol)
org5.ptx.TS <- extract_timestamps(df5.PTX)
#now actually use the timestamps info.
eff.SR <- 19254 #real.SR <- 19754 #sampling rate #effective.SR <- real.SR - 500
org5.control.TS$Real.Time <- org5.control.TS$Time / eff.SR # is this correct? think about this a wee bit more...
org5.propofol.TS$Real.Time <- org5.propofol.TS$Time / eff.SR
org5.ptx.TS$Real.Time <- org5.ptx.TS$Time / eff.SR
#make graph
graphe <- make_raster_plot(org5.control.TS, "Raster Plot - P60 org5 Control: PTX-selection", "Control")
graphe
#save_graph(graphe, "org5- p60 control raster plot PTX-selection .pdf")
graphf <- make_raster_plot(org5.propofol.TS, "Raster Plot - P60 org5 Propofol: PTX-selection", "Propofol")
graphf
#save_graph(graphf, "Org5- p60 propofol raster plot PTX-selection .pdf")
graphg <- make_raster_plot(org5.ptx.TS, "Raster Plot - P60 org5 PTX: PTX-selection", "PTX")
graphg
#save_graph(graphg, "Org5- p60 ptx raster plot PTX-selection.pdf")

# P60 org6####
setwd("/Users/chloe/Documents/R/Organoids Results/Organoid Results P60/")
org6_control_timestamps <- read.delim("org6_control_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
org6_propofol_timestamps <- read.delim("org6_03_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
org6_ptx_timestamps <- read.delim("org6_05_ptx_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
#rename the channels.
#import the channels
org6.channels <- read.table("OrganoidChannels6.txt", header = T, sep="\t")
new_column_names <- org6.channels$X1
colnames(org6_control_timestamps) <- as.character(new_column_names)
colnames(org6_propofol_timestamps) <- as.character(new_column_names)
colnames(org6_ptx_timestamps) <- as.character(new_column_names)
#Then select just the channels I want.
#they are in org6_p60_lowPTX
channels.I.want <- org6_p60_lowPTX$Channels
#select only the channels I want from these timestamp tables
df6.control <- org6_control_timestamps[, colnames(org6_control_timestamps) %in% channels.I.want]
df6.propofol <- org6_propofol_timestamps[, colnames(org6_propofol_timestamps) %in% channels.I.want]
df6.PTX <- org6_ptx_timestamps[, colnames(org6_ptx_timestamps) %in% channels.I.want]
#what input does the extract_timestamps function need??
#then extract the timestamps...
#---------------------------------------------------------------------------------#
# clean up the messy data, extract the timestamps from the chunked file #
# and make it continuous
org6.control.TS<- extract_timestamps(df6.control)
org6.propofol.TS <- extract_timestamps(df6.propofol)
org6.ptx.TS <- extract_timestamps(df6.PTX)
#now actually use the timestamps info.
eff.SR <- 19254 #real.SR <- 19754 #sampling rate #effective.SR <- real.SR - 500
org6.control.TS$Real.Time <- org6.control.TS$Time / eff.SR # is this correct? think about this a wee bit more...
org6.propofol.TS$Real.Time <- org6.propofol.TS$Time / eff.SR
org6.ptx.TS$Real.Time <- org6.ptx.TS$Time / eff.SR
#make graph
graphh <- make_raster_plot(org6.control.TS, "Raster Plot - P60 org6 Control: PTX-selection", "Control")
graphh
#save_graph(graphh, "org6- p60 control raster plot PTX-selection .pdf")
graphi<- make_raster_plot(org6.propofol.TS, "Raster Plot - P60 org6 Propofol: PTX-selection", "Propofol")
graphi
#save_graph(graphi, "org6- p60 propofol raster plot PTX-selection .pdf")
graphj<- make_raster_plot(org6.ptx.TS, "Raster Plot - P60 org6 PTX: PTX-selection", "PTX")
graphj
#save_graph(graphj, "org6- p60 ptx raster plot- PTX selection.pdf")


##org 2 P60 ####
setwd("/Users/chloe/Documents/R/Organoids Results/Organoid Results P60/")
org2_propofol_timestamps <- read.delim("org2_05_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
org2_control_timestamps <- read.delim("org2_01_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
org2_ptx_timestamps <- read.delim("org2_ptx_Timestamps_of_spikes_60.txt", header = F, sep = "\t", dec = ".")
#rename the channels.
#import the channels
org2.channels <- read.table("Organoid2_channels.txt", header = T, sep="\t")
new_column_names <- org2.channels$X1
colnames(org2_control_timestamps) <- as.character(new_column_names)
colnames(org2_propofol_timestamps) <- as.character(new_column_names)
colnames(org2_ptx_timestamps) <- as.character(new_column_names)
#Then select just the channels I want.
#they are in org6_p60_lowPTX
channels.I.want <- org2_p60_lowPTX$Channels
#select only the channels I want from these timestamp tables
df2.control <- org2_control_timestamps[, colnames(org2_control_timestamps) %in% channels.I.want]
df2.propofol <- org2_propofol_timestamps[, colnames(org2_propofol_timestamps) %in% channels.I.want]
df2.PTX <- org2_ptx_timestamps[, colnames(org2_ptx_timestamps) %in% channels.I.want]
#what input does the extract_timestamps function need??
#then extract the timestamps...
#---------------------------------------------------------------------------------#
# clean up the messy data, extract the timestamps from the chunked file #
# and make it continuous
org2.control.TS<- extract_timestamps(df2.control)
org2.propofol.TS <- extract_timestamps(df2.propofol)
org2.ptx.TS <- extract_timestamps(df2.PTX)
#now actually use the timestamps info.
eff.SR <- 19254 #real.SR <- 19754 #sampling rate #effective.SR <- real.SR - 500
org2.control.TS$Real.Time <- org2.control.TS$Time / eff.SR # is this correct? think about this a wee bit more...
org2.propofol.TS$Real.Time <- org2.propofol.TS$Time / eff.SR
org2.ptx.TS$Real.Time <- org2.ptx.TS$Time / eff.SR
#make graph
graph.k <- make_raster_plot(org2.control.TS, "Raster Plot - P60 org2 Control: PTX-selection", "Control")
graph.k
save_graph(graph.k, "org2- p60 control raster plot ptx-selection.pdf")
graph.l<- make_raster_plot(org2.propofol.TS, "Raster Plot - P60 org2 Propofol: PTX-selection", "Propofol")
graph.l
save_graph(graph.l, "org2- p60 propofol raster plot ptx-selection.pdf")
graph.m <- make_raster_plot(org2.ptx.TS, "Raster Plot - P60 org2 PTX: PTX-selection", "PTX")
graph.m
save_graph(graph.m, "org2- p60 ptx raster plot ptx-selection.pdf")
