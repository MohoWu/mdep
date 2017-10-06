\name{zeroSpeeds}

\alias{replaceZeroNaMIDAS}


\title{
deals with zero speeds
}
	
\description{
one way to deal with zero speeds so it helps in the calculation
of traffic densities \link{macroMIDAS}. The method is to replace all zero speeds 
by NA values and so traffic density values would be NA as well. 
}
	
\usage{
##replace zeros with NA
replaceZeroNaMIDAS(x, speed.lane = "Speed.Lane")
}
	
\arguments{
\item{x}{
	object of class 'midas' with speed column(s)
	}
\item{speed.lane}{
	character. part of names of the speed columns 
	which distinguish them from other columns. Default to "Speed.Lane"
	}
}

\value{
midas object with updated speed data of the traffic.data element
(zeros replaced by NA's)
}

\note{
not sure if this is the right way to do it
for some entries, zero speed does not make sense, more like a measurement fault
}
	

\seealso{
\link{macroMIDAS}
}
	
\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- replaceZeroNaMIDAS(x = md)
#comment(ex) #provides the number of rows where zero speeds are found and replaced by NA
}}
