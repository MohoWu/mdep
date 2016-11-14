\name{createMIDAS}

\alias{createMIDAS}

\title{
creates a midas object
}
	
\description{
creates a midas object of the midas traffic data files. 
}
	
\usage{
createMIDAS(data, date = "Date", date.format = NULL)
	}

\arguments{
\item{data}{
	data frame. midas traffic data (as read from .csv after download)
	}
\item{date}{
	character. name of date/datetime column
	}
\item{date.format}{
	format of date column. Default is NULL (change if not in the form "\%d/\%m/\%y")
	}
}

\value{
object of class 'midas'
\item{traffic.data}{traffic data for the specified date(s)}
\item{collection.date}{date and day of the week of the traffic data}
}
	
\seealso{
\link{readMIDAS}
}
	
\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- createMIDAS(data = md)
}}
