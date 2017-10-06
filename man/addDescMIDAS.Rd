\name{addDescMIDAS}

\alias{addDescMIDAS}

\title{
adds a description of the loop detector location
}

\description{
extracts the name of the road/location/code/direction 
of each loop detector in the 'midas' object
}

\usage{
addDescMIDAS(x, geo.address = "Geographic.Address")
	}

\arguments{
\item{x}{
	object of class 'midas'
	}	
\item{geo.address}{
	character. name of the geographic address columns.
	Default to "Geographic.Address"
	}
}

\value{
'midas' object with 4 columns added to the traffic.data element. 
These are the road at which the loop detector is installed, 
the location (onramp/offramp/mainline), the direction (anticlockwise/clockwise), 
and the code of each corresponding geographic address 
which would help in route identification 
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- addDescMIDAS(x = md)
}}
