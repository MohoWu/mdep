\name{generics.midas}
\alias{as.data.frame.midas}
\alias{names.midas}
\alias{print.midas}
\alias{summary.midas}
\alias{head.midas}
\alias{tail.midas}

\title{
generic functions for midas
}
	
\description{
data.frame, names, print, summary, head, and tail generic functions 
used for object of class 'midas'
}
	
\usage{
\method{as.data.frame}{midas}(x, ...)
\method{names}{midas}(x)
\method{print}{midas}(x, ..., n = 5)
\method{summary}{midas}(object,...)
\method{head}{midas}(x, ..., n = 5)
\method{tail}{midas}(x, ..., n = 5)	
}
	
\arguments{
\item{x, object}{
	midas object
	}
\item{\dots}{
	additional arguments to default s3 methods
	}
\item{n}{
	numeric. number of rows to print, or to show when using head or tail
	}
}
	
\value{
traffic data (dataframe) is extracted using as.data.frame()
get or set column names of the traffic data (data frame) using names()
print the date and sample of the 'midas' taffic data using print()
get a summary of the traffic data (dataframe) using summary()
print the first n rows of the traffic data (dataframe) using head()
print the last n rows of the traffic data (dataframe) using tail()
}

\examples{
\dontrun{
#md is a one .csv data set you've downloaded
m <- as.data.frame(x = md)
head(m)
#summary of midasO.ex data frame
summary(object = md)
#head of midasO.ex data frame
head(x = md)
}}