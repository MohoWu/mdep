\name{cutTimeMIDAS}

\alias{cutTimeMIDAS}

\title{
cuts data to selected time period 
}

\description{
midas traffic data are 24 hour data for a specified data. 
Users might need to analyse specific time period within a day.
This function allow data to be cut based on time period.
}

\usage{
cutTimeMIDAS(x, start.time, end.time, datetime = "datetime")
}

\arguments{
	\item{x}{
		object of class 'midas'
		}
	\item{start.time, end.time}{
		numeric. start and end time in hours of the selected period. 
		If end time is 18 then 18.00 is the last entry extracted.
		}
	\item{datetime}{
		character. column name of the datetime (check \link{datetimeMIDAS}).
		Default to "datetime"
		}
	}

\value{
'midas' object cut to the specified time period
}

\seealso{
\link{datetimeMIDAS}, \link{routeMIDAS}
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- datetimeMIDAS(x = md)
sub.ex <- cutTimeMIDAS(x = ex, start.time = 2.5, end.time = 5.3)
}}