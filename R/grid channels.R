#Script to make the grids of channels.
#to work on just one channel
channel_to_index <- function(Ch){
  max.full.rows <- floor(Ch / 64 )
  col.idx <- Ch - (max.full.rows*64)
  row.idx <- (max.full.rows+1)
  Ch.idx <- c(row.idx, col.idx)
  return(Ch.idx)
}
#to work on a list, not a single channel ####
channels_to_index <- function(Ch_list) {
  max.full.rows <- floor(Ch_list / 64 )
  col.idx <- Ch_list - (max.full.rows * 64)
  row.idx <- (max.full.rows + 1)
  Ch.idx <- cbind(row.idx, col.idx)
  return(Ch.idx)
}
# ^^ Already have this function to convert the raw number to the Ch.
# ####

# so I want to get all the channels out per organoid.
#let's begin with organoid 2, p60
o2.p60.ptxselect<- org2_p60_lowPTX$Channels
#now convert these to channel index.
test1<- channels_to_index(o2.p60.ptxselect)
test1 <- as.data.frame(test1)

# Create a 64 x 64 matrix with zeros
grid_matrix <- matrix(0, nrow = 64, ncol = 64)
# Loop through each row of the data frame
for (i in 1:nrow(test1)) {
  row_idx <- test1$row.idx[i]
  col_idx <- test1$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  grid_matrix[row_idx, col_idx] <- 1
}
#now find the channels from the propofol conditions
#let's stay with organoid 2, p60. # Get channels and convert to indices for lowProp
o2.p60.propofol.select<- org2_p60_lowProp$Channels
df.o2.young <- channels_to_index(o2.p60.propofol.select)
df.o2.young <- as.data.frame(df.o2.young)

# Loop through each row of the data frame for lowProp
for (i in 1:nrow(df.o2.young)) {
  row_idx <- df.o2.young$row.idx[i]
  col_idx <- df.o2.young$col.idx[i]
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  grid_matrix[row_idx, col_idx] <- ifelse(grid_matrix[row_idx, col_idx] == 1, 3, 2)
}

# Plot the heatmap
heatmap(grid_matrix, Rowv = NA, Colv = NA, col = c("white", "purple", "green", "yellow"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "Organoid 2 P60")

# Add a red border around the highlighted cell
rect(col_idx - 0.5, row_idx - 0.5, col_idx + 0.5, row_idx + 0.5, border = "red", lwd = 2)
