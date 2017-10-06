\name{routeMIDAS}

\alias{routeMIDAS}

\title{
cuts data to a specified route
}

\description{
traffic data files are downloaded for a wide area with hundreds of loop detector. 
This function cuts data to the route (e.g. continuous section of a road or a specific road) 
users would like to study.
}

\usage{
routeMIDAS(x, road, traffic.bound = c("clockwise","anticlockwise"), 
	start.pt = NULL, end.pt = NULL, 
	location.info = c("mainline", "onramp","offramp"),
	description = c("road","code","direction","location"), ...,
	drop.onramp.duplicate = NULL, drop.offramp.duplicate = NULL)
}

\arguments{
	\item{x}{
		object of class 'midas'
		}
	\item{road}{
		character. road to be studied e.g. "M25" or "A2"
		}
	\item{traffic.bound}{
		character. "clockwise" or "anticlockwise" section of the road. 
		Default to both  c("clockwise", "anticlockwise")
		}
	\item{start.pt, end.pt}{
		code of the selected start (e.g. "4860") and end (e.g. "4909") points. 
		If NULL, then all specified roads are selected. Default to NULL
		}
	\item{location.info}{
		character. there are three options for location of loop detector 
		(mainline, on-ramp, or off-ramp).
		if mainline data for a section is needed, then specify as "mainline". 
		Default to c("mainline", "onramp", "offramp")
		}
	\item{description}{
		character. names of the road, code, direction, and location columns
		which are created using \link{addDescMIDAS}. Change if the column names are changed.
		Default to c("road", "code", "direction", "location"). 
		If these are not created, 
		the function automatically uses \link{addDescMIDAS} to add these
		}
	\item{\dots}{
		optional. additional arguments to addDescMIDAS()
		}
	\item{drop.onramp.duplicate, drop.offramp.duplicate}{
		traffic data sometime has duplicate geographic address for the same location 
		or multiple loop detectors on the same on/off ramp. 
		If any needs to be droped then specify their codes (e.g. "4890")
		}
	}

\value{
'midas' object cut to the specified route
}

\seealso{
\link{addDescMIDAS}, \link{cutTimeMIDAS}
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
#select a 5 km route on the anticlockwise of M25 motorway starting 
#from the loop detector 4909 and ending at 4860. Only mainline data are extracted
ex <- addDescMIDAS(x = md)
sub.ex <- routeMIDAS(x = ex, road = "M25", traffic.bound = "anticlockwise",
		start.pt = "4909", end.pt = "4860", location.info = c("mainline","onramp"))
}}