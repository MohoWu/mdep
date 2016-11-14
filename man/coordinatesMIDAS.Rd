\name{coordinatesMIDAS}

\alias{coordinatesMIDAS}

\title{
adds lat/lon coordinates of the loop detectors
}

\description{
each geographic address has a unique lat/lon given in the coordinates data 
which are extracted from the NTIS model. The coordinates might 
not necessarily reflect location of measurement sites on pavement.
The function adds lat/lon of the specific geographic addresses in the 'midas' object.
This could be used in order to plot locations
on maps using other map plotting packages/functions.
}
	
\usage{
coordinatesMIDAS(x, coordinates, 
		geo.address = "Geographic.Address")
	}

\arguments{
\item{x}{
	object of class midas with geographic address column
	}
\item{coordinates}{
	data.frame with three columns: first is geographic address, second is latitudes, third is longitude
	}
\item{geo.address}{
	character. names of the geographic address column. Default to "Geographic.Address"
	}
}

\value{
'midas' object with lat/lon column added to the traffic.data element
}