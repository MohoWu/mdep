\name{rbindMIDAS}

\alias{rbindMIDAS}

\title{
binds and orders multiple objects of class 'midas'
}

\description{
function to combine midas objects
and order values based on geographic address or address and date
}
	
\usage{
rbindMIDAS(x, order = TRUE, 
	geo.address = "Geographic.Address", date.time = "datetime", 
	date = NULL, time = NULL)
	}

\arguments{
\item{x}{
	list of 'midas' objects
	}
\item{order}{
	logical whether to order combined traffic data or not. Default to TRUE
	}
\item{geo.address, date.time}{
	character. name of geographic address and datetime columns.
	Default to "Geographic.Address" and "datetime", respectively
	not necessary (NULL) if order is set to FALSE
	}
\item{date, time}{
	if datetime is not created either create using \link{datetimeMIDAS} or 
	give the names of the date and time columns e.g. "Date"/"Time". 
	Default to NULL. assumes datetime column is created.
	}
}

\value{
single midas object with modified collection.date 
to account for all the midas object combined
}
	
\note{
midas object needs to have same columns (nr of columns and names)
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
ex <- datetimeMIDAS(x = md)
ex2 <- rbindMIDAS(x = list(ex, ex))
}}