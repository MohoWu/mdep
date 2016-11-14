\name{macroMIDAS}

\alias{macroMIDAS}

\title{
calculates macroscopic traffic variables
}

\description{
calculates macroscopic traffic variables of average flows, average speeds, 
average occupancies, and average densities from lane flows and lane speed data 
and removes all per lane data if cut.macro is set to TRUE.
NOTE: Speeds per lane are based on the arithmetic mean of individual vehicles, 
not the harmonic mean. So average speeds at the end are the time-mean speeds 
rather than space-mean speed. It is hard to derive space-mean speeds 
when all we have is arithmetic means especially during congested conditions 
where speed variance is very high.
}

\usage{
macroMIDAS(x, cut.macro = TRUE, replace.zero.spd = NULL, ..., 
	lane.nr = "Number.of.Lanes", speed.lane = "Speed.Lane",
	spd.method = "weighted",
	flow.cat = "Flow.Category", flow.lane = "Flow.Lane", 
	occ.lane = "Occupancy.Lane", hw.lane = "Headway.Lane")
	}

\arguments{
\item{x}{
	object of class 'midas' and flow/speed data per lane
	}
\item{cut.macro}{
	logical whether to remove per lane data or not. Default to TRUE
	}
\item{replace.zero.spd}{
	the method to replace zero speeds in each lane. 
	currently only \link{replaceZeroNaMIDAS} to replace by NA. Default to NULL
	}
\item{\dots}{
	additional arguments to replaceZeroSpd()
	}
\item{lane.nr, speed.lane, flow.cat, 
	flow.lane, occ.lane, hw.lane}{
	character. part of the 
	flow per category/flow per lane/speed per lane/occupancy per lane/headway per lane
	column names which distinguish them from other columns
	}
\item{spd.method}{
	character. method for calculating average speed
	"weighted" for weighted arithmetic average speed
	"non-weighted" for arithmetic average speed
	"harmonic" for approximate harmonic average speed as suggested by Poole and Kotsialos (2012, 2016). Default to "weighted".
	}
}
	
\value{
	midas object with the traffic.data element modified. Macroscopic variable added.
	average flows over all lanes in vehicles per hour
	average flows per lane in vehicles per hour per lane
	average speeds in km per hour
	average occupancy in percentage
	average densities over all lanes in vehicles per km
	average densities per lane in vehicles per km per lane
}

\references{
	##need to add ref
	}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- datetimeMIDAS(x = md)
ex <- cutTimeMIDAS(x = ex, start.time = 9, end.time = 10)
ex1 <- macroMIDAS(x = ex , cut.macro = FALSE)
ex2 <- macroMIDAS(x = ex, cut.macro = FALSE, spd.method = "harmonic")
plot(ex1$traffic.data$avg.spd, ex2$traffic.data$avg.spd)
plot(ex1$traffic.data$avg.den, ex2$traffic.data$avg.den)
}}