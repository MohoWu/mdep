\name{reduceMIDAS}

\alias{reduceMIDAS}

\title{
removes NA columns
}

\description{
default midas traffic data is flow/speed/occupancy/headway per lane for 7 lanes.
some columns are all NA values because some sections do not have 7 lanes. Function removes these columns.
}
	
\usage{
reduceMIDAS(x, col.removed = TRUE)
}

\arguments{
\item{x}{
	object of class 'midas'
	}
\item{col.removed}{
	logical. if true then a comment on the removed columns will be returned.
	Default to true.
	}
}

\value{
'midas' object with removed NA columns.
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- reduceMIDAS(x = md)
comment(ex) #returns name of columns removed
}}