\name{nrow}
\alias{nrow}
\alias{nrow.default}
\alias{nrow.midas}
\alias{ncol}
\alias{ncol.default}
\alias{ncol.midas}

\title{
returns the number of rows or columns
}

\description{
nrow/ncol are base non generic functions. Here nrow/ncol are made generic to be used for the midas 'class'
}

\usage{
\method{nrow}{midas}(x)
\method{ncol}{midas}(x)
}

\arguments{
	\item{x}{
		object of class 'midas'
		}
	}

\value{
an integer of length 1 or NULL representing nrow/ncol of 'midas' dataframe
}