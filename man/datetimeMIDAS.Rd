\name{datetimeMIDAS}
\alias{datetimeMIDAS}

\title{
converts Date and Time to class POSIXct/POSIXt
}
	
\description{
function to create a date/time column format based on Date and Time columns of a 'midas' object
}
	
\usage{
datetimeMIDAS(x, date = "Date", time = "Time", 
	date.format = NULL, 
	remove.date = TRUE, remove.time = TRUE)
	}

\arguments{
\item{x}{
	object of class 'midas'
	}
\item{date, time}{
	character. name of the date and time columns. 
	Default (in downloaded file) to "Date"/"Time", respectively
	}
\item{date.format}{
	format of date and time columns. Default to "\%d/\%m/\%y \%H:\%M"
	}
\item{remove.date, remove.time}{
	logical to remove original Date/Time columns. Default to TRUE
	}
}

\value{
midas object with datetime column of class "POSIXct/POSIXt" 
in its traffic.data element 
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- datetimeMIDAS(x = md)
}}
