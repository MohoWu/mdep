\name{readMIDAS}

\alias{readMIDAS}

\title{
reads and creates a MIDAS object
}

\description{
function to read downloaded MIDAS .csv file and create a MIDAS object
}
	
\usage{
readMIDAS(file.name = file.choose(), 
	col.classes.m = c("character", rep("NULL", 4), rep("factor", 2), rep("numeric", 33)), 
	na.strings.m = "-1", ..., args.createMIDAS = list())
	}

\arguments{
\item{file.name}{
	.csv traffic data file to be read
	}
\item{col.classes.m}{
	vector. classes of the 40 columns of midas traffic data.
	first column is the Geograhic.Address,
	4 NULL columns are details of the CO/LCC/transponder/Device address,
	change to "character" if information needed.
	Date and Time as factors, see \link{datetimeMIDAS} to change to date/time class
	all other columns are number of lanes/flow per category/
	speed per lane/flow per lane/occupancy per lane/headway per lane 
	at each geographic address.
	}
\item{na.strings.m}{
	character. na value in the midas traffic data are set to "-1"
	}
\item{\dots}{
	?read.csv for information
	}
\item{args.createMIDAS}{
	list. additional arguments to createMIDAS function
	}
}

\value{
reads the downloaded and decoded midas traffic data file and creates a midas object
which is a list with traffic.date and collection date elements
}

\seealso{
\link{createMIDAS}
}
	
\examples{
\dontrun{
system.time(ex <- readMIDAS(file.name = file.choose()))
system.time(ex <- readMIDAS(file.name = paste(getwd(), "/70010410.tcd.csv", sep = "")))
head(ex)
}}