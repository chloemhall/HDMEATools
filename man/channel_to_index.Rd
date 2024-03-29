% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions .R
\name{channel_to_index}
\alias{channel_to_index}
\title{channel_to_index}
\usage{
channel_to_index(df1)
}
\arguments{
\item{df1}{is the table with the list of integers "Channels" of the electrodes.}
}
\value{
returns new df with a useful index of rows and columns, listed in separate columns.
}
\description{
Converts a simple integer of channel into more useful index, for a list of channels in df1.
( on 3Brain Accura chips, this is 64 rows x 64 columns. eg. channel 2 is row 1, col 2 ie. 1.2 )
}
\examples{
> new.df<- channel_to_index(test.dataframe)
> print(new.df)
>
}
