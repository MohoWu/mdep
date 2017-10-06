\name{wideMacroMIDAS}

\alias{wideMacroMIDAS}

\title{
reshapes from long to wide data frame
}

\description{
uses the reshape stats function to reshape 'midas' dataframe from long format to wide format. 
}

\usage{
wideMacroMIDAS(x, code = "code", 
	datetime = "datetime", 
	vars = c("avg.flow", "avg.flow.per.lane", "avg.spd", "avg.den", "avg.den.per.lane"),...,
	traffic.bound = NULL)
}

\arguments{
	\item{x}{
		object of class 'midas'
		}
	\item{code}{
		character. name of the code column. 
		codec is the variable in long format that differentiates multiple records 
		(?reshape for more info) 
		Default to "code"
		}
	\item{datetime}{
		character. datetime column name. 
		datetimec is names of one or more variables in long format 
		that identify multiple records from the same group/individual.
		(?reshape for more info)
		Default to "datetime" (\link{datetimeMIDAS}) 
		}
	\item{vars}{
		character. names of variables in the long format that correspond 
		to multiple variables in the wide format
		(?reshape for more info)
		Default to c("avg.flow", "avg.flow.per.lane", "avg.spd", "avg.den", "avg.den.per.lane") (\link{macroMIDAS}) 
		}
	\item{\dots}{
		optional additional arguments to stats::reshape
		}
	\item{traffic.bound}{
		(optional) character. if midas object has only anticlockwise traffic data then 
		set to anticlockwise to order the dataframe from start point to end point 
		(because the code numbers for both directions are the same). 
		}

}

\value{
list containing the wide format data frame(s) for each variable in 'vars' argument.
}

\seealso{
\link{macroMIDAS}, \link{addDescMIDAS}, \link{datetimeMIDAS}
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- datetimeMIDAS(x = md)
ex <- cutTimeMIDAS(x = ex, start.time = 9, end.time = 10)
ex <- macroMIDAS(x = ex, spd.method = "weighted")
sub.ex <- routeMIDAS(x = ex, road = "M25", traffic.bound = "anticlockwise",
		start.pt = "4909", end.pt = "4860", location.info = c("mainline","onramp"))
mflows <- wideMacroMIDAS(sub.ex, vars = "avg.flow", traffic.bound = "anticlockwise")
}}