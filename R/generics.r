#need to add [], $

#S3 methods for class 'midas'
#as.data.frame S3 method for class 'midas'
as.data.frame.midas <- function (x, ...) {
		dd <- x$traffic.data
		return(dd)
	}

#names S3 method for class 'midas'
names.midas <- function (x) {
		dd <- x$traffic.data
		return(names(dd))
	}

#print S3 method for class 'midas'
print.midas <- function (x, ..., n = 5) {
		cat("$date(s)\n")
		print(x$collection.date[[1]])
		cat("\n$day(s)\n")
		print(x$collection.date[[2]])
		cat("\n$Sample\n")
		print(head(x$traffic.data, n=n),...)
	}

#summary S3 method for class 'midas'	
summary.midas <- function (object,...) {
	dd <- object$traffic.data
	return(summary(dd))
	}

#head S3 method for class 'midas'
head.midas <- function(x, ...,  n = 5) {
	dd <- x$traffic.data
	return(dd[1:n,])
	}

#tail S3 method for class 'midas'
tail.midas <- function(x, ..., n = 5) {
	dd <- x$traffic.data
	return(dd[(nrow(dd) - n + 1):nrow(dd),])
	}

#make nrow generic function
#create default for nrow - simple
#point the default method to the function nrow	
#nrow method for class 'midas'
nrow <- function(x) UseMethod("nrow")
nrow.default <- function(x) 
	stop("No method associated with this class")
nrow.default <- function(x) base::nrow(x)

nrow.midas <- function(x) {
	dd <- x$traffic.data
	return(nrow(dd))
	}

#make ncol generic function
#create default for ncol - simple
#point the default method to the function ncol		
#ncol method for class 'midas'
ncol <- function(x) UseMethod("ncol")
ncol.default <- function(x) 
	stop("No method associated with this class")
ncol.default <- function(x) base::ncol(x)

ncol.midas <- function(x) {
	dd <- x$traffic.data
	return(ncol(dd))
	}
