\name{calcMinAvgSpdMIDAS}

\alias{calcMinAvgSpdMIDAS}

\title{
calculates average minimum speeds
}

\description{
calculates average minimum speeds for each loop detector in a 'midas' object
}

\usage{
calcMinAvgSpdMIDAS(x, geo.address = "Geographic.Address", 
		speed = "avg.spd", traffic.bound = NULL)
}

\arguments{
	\item{x}{
		object of class 'midas'
		}
	\item{geo.address, speed}{
		character. geographic address and average speed column names. 
		Default to "Geographic.Address" and "avg.spd", respectively
		}
	\item{traffic.bound}{
		(optional) character. if midas object has only anticlockwise traffic data then 
		set to anticlockwise to order the dataframe from start point to end point 
		(because the code numbers for both directions are the same). 
		}
}

\value{
list
\item{min.avg.spd}{dataframe containing the geographic address 
		and its corresponding observed minimum speed}
\item{avg.min.avg.spd}{average minimum over all loop detectors}
\item{min.min.avg.spd}{minimum observed over all loop detectors}
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- macroMIDAS(x = md, cut.macro = FALSE, spd.method = "weighted",
		replaceZeroSpd = replaceZeroNaMIDAS)
minSpds <- calcMinAvgSpdMIDAS(x = ex)
}}