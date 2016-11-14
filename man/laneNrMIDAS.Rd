\name{laneNrMIDAS}

\alias{laneNrMIDAS}

\title{
extracts number of lanes
}

\description{
extracts number of lanes of the road at which loop detectors located
}

\usage{
laneNrMIDAS(x, geo.address = "Geographic.Address", lane.nr = "Number.of.Lanes", 
	direction.info = "direction", location.info = "location", 
	traffic.bound = NULL)
}

\arguments{
	\item{x}{
		object of class 'midas'
		}
	\item{geo.address, lane.nr, direction.info, location.info}{
		character. column names of the geographic address, 
		number of lanes, direction, and location.
		Default to "Geographic.Address", "Number.of.Lanes", 
		"direction", and "location", respectively 
		check \link{addDescMIDAS}
		}
	\item{traffic.bound}{
		(optional) character. if midas object has only anticlockwise traffic data then 
		set to anticlockwise to order the dataframe from start point to end point 
		(because the code numbers for both directions are the same). 
		}
	}

\value{
dataframe of four columns (geographic address, direction of the geographic address,
location of the geographic address, and number of lanes for the corresponding geographic address)
}

\seealso{
\link{addDescMIDAS}, \link{routeMIDAS}
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- addDescMIDAS(x = md)
l <- laneNrMIDAS(x = ex)
}}