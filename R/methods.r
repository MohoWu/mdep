#uses the Date and Time column in the original file and transforms them to class POSIXlt/POSIXct
#to use when creating MIDAS
datetimeMIDAS <- function(x, date = "Date", time = "Time", 
						date.format = NULL,
						remove.date = TRUE, remove.time = TRUE) { 
	#need Date and Time columns 
	if(!date %in% names(x) || !time %in% names(x))
		stop("date and/or time columns are missing in your data")
	#extract dataframe from object of class 'midas'
	dd <- as.data.frame(x)
	#add a new datetime column
	if(is.null(date.format)) date.format <- "%d/%m/%y %H:%M"
	dd$datetime <- 
		as.POSIXct(paste(dd[,date], " ", dd[,time]), 
                format = date.format, "GMT")
	#optional to remove original Date column
	if(remove.date == TRUE) dd[,date] <- NULL
	#optional to remove original Time column
	if(remove.time == TRUE) dd[,time] <- NULL
	
	x$traffic.data <- dd
	return(x)
	}
	
#remove columns which are all NA's
reduceMIDAS <- function(x, col.removed = TRUE) { 
	dd <- as.data.frame(x)
	#identify column names to be removed
	col.rm <- names(which((colSums(is.na(dd)) < nrow(dd)) == FALSE))
	#remove columns
	dd <- dd[,colSums(is.na(dd)) < nrow(dd)]
	x$traffic.data <- dd
	#add comment of the columns removed
	if(col.removed == TRUE) comment(x) <- col.rm
	return(x)
	}
		
#bind rows for class 'midas'
#x is a list of midas objects each with datetime column
rbindMIDAS <- function(x, order = TRUE, 
					geo.address = "Geographic.Address", date.time = "datetime", 
					date = NULL, time = NULL) {
		#extract dataframe from objects of class 'midas'
		dd <- lapply(x, as.data.frame)
		#rbind the data frames
		dd <- do.call(rbind, dd)
		#order based on geographic address and date.time column
		if(order == TRUE && !geo.address %in% colnames(dd))
			stop("geo.address column does not exist")
		if(order == TRUE && !is.null(date.time) && !date.time %in% colnames(dd))
			stop("date.time column does not exist")			
		if(order == TRUE && !is.null(date.time))
			dd <- dd[order(dd[,geo.address], dd[,date.time]),]
		#if date.time == NULL  and date and time column are given 
		#then ordering based on date and time separately
		if(order == TRUE && !is.null(date) && !is.null(time) && !c(date,time) %in% colnames(dd))
			stop("date and/or time column does not exist")	
		if(order == TRUE && !is.null(date) && !is.null(time))
			dd <- dd[order(dd[,geo.address], dd[,date], dd[,time]),]
		#create midas object again
		if(!is.null(date.time)) x <- createMIDAS(data = dd, date = date.time)
		if(!is.null(date)) x <- createMIDAS(data = dd, date = date)
		return(x)
		}

#zero speed create infinite values when calculating traffic density
#one suggestion is to replace zero in each speed data column (for each lane) by NA
#not sure if this is the right way to do it
#for some entries, zero speed does not make sense, more like a measurement fault
replaceZeroNaMIDAS <- function(x, speed.lane = "Speed.Lane") {
	#extract dataframe from object of class 'midas'
	dd <- as.data.frame(x)
	#grep all speed columns
	spd <- dd[,grep(speed.lane, colnames(dd))]
	#identify row columns of zero speeds
	spd.zero.row <- sapply(seq(spd), function(i) which(spd[,i] ==0))
	spd.zero.col <- unlist(sapply(seq(spd), function(i) {
				temp <- which(spd[,i] ==0)
				if(length(temp) > 0) return(i)
				}))
	#replace by NA
	if(length(unlist(spd.zero.row)) > 0) {
		spd <- sapply(spd.zero.col, function(i) {
			 spd[spd.zero.row[[i]],i] <- NA
			return(spd[,i])
			})
		}
	dd[,grep(speed.lane, colnames(dd))] <- spd
	#modify data frame
	x$traffic.data <- dd
	#add comment of row numbers of zero speeds
	#returning no visible binding to global variable 'zero.spd.row', return the comment in a listed output instead?
	#if(length(unlist(spd.zero.row)) > 0) comment(x) <- as.character(unlist(zero.spd.row)) 
	#if(length(unlist(spd.zero.row)) == 0) comment(x) <- "zero speeds not available"
	return(x)
	}

#calculate macroscopic variables based
#average speed can be calculated in three ways here
#non-weighted arithmetic average speed, weighted arithmetic average speed, approximate harmonic average speed
#default to weighted
macroMIDAS <- function(x, cut.macro = TRUE, replace.zero.spd = NULL, ..., 
					lane.nr = "Number.of.Lanes", speed.lane = "Speed.Lane",
					spd.method = "weighted", 
					flow.cat = "Flow.Category", flow.lane = "Flow.Lane", 
					occ.lane = "Occupancy.Lane", hw.lane = "Headway.Lane") {
		if(!is.null(replace.zero.spd)) x <- replace.zero.spd(x, ...)
		#extract dataframe from object of class 'midas'
		dd <- as.data.frame(x)
		#calculate average flows
		dd$avg.flow <- rowSums(dd[,grep(flow.lane, colnames(dd))], na.rm=TRUE)*60
		dd$avg.flow.per.lane <- dd$avg.flow/dd[,lane.nr]
		
		if(spd.method == "weighted") {
		#calculate weighted average speeds
			s <- dd[,grep(speed.lane, colnames(dd))]
			f <- dd[,grep(flow.lane, colnames(dd))]
			f[is.na(s)] <- NA
			dd$avg.spd <- rowSums(s*f, na.rm=TRUE)/rowSums(f, na.rm=TRUE)

		} else if(spd.method == "non-weighted") {
			#else arithmetic means
			dd$avg.spd <- rowMeans(dd[,grep(speed.lane, colnames(dd))], na.rm=TRUE)
		
		} else if(spd.method == "harmonic") {
		#calculate harmonic speeds as suggested by poole for comparison
			s <- dd[,grep(speed.lane, colnames(dd))]
			f <- dd[,grep(flow.lane, colnames(dd))]
			f[is.na(s)] <- NA
			dd$avg.spd <- rowSums(f, na.rm=TRUE)/rowSums(f/s, na.rm=TRUE)
		}

		#if all speed per lane are NA
		dd$avg.spd <- replace(dd$avg.spd, is.na(dd$avg.spd), NA)
		#calculate average occupancies
		dd$avg.occ <- rowMeans(dd[,grep(occ.lane, colnames(dd))], na.rm=TRUE)
		#if all occupancy per lane are NA
		dd$avg.occ <- replace(dd$avg.occ, is.na(dd$avg.occ), NA)
		#calculate average headways
		dd$avg.hw <- rowMeans(dd[,grep(hw.lane, colnames(dd))], na.rm=TRUE)
		#if all headway per lane are NA
		dd$avg.hw <- replace(dd$avg.hw, is.na(dd$avg.hw), NA)
		#calculate average densities
		dd$avg.den <- dd$avg.flow/dd$avg.spd
		dd$avg.den.per.lane <- dd$avg.flow.per.lane/dd$avg.spd
		#if midas object to be cut to macroscopic variables only 
		#remove all per lane columns
		#modify description
		if(cut.macro == TRUE) {
			drops <- c(if(!is.null(flow.cat)) grep(flow.cat, colnames(dd)),
					if(!is.null(flow.lane)) grep(flow.lane, colnames(dd)),
					if(!is.null(speed.lane)) grep(speed.lane, colnames(dd)),
					if(!is.null(occ.lane)) grep(occ.lane, colnames(dd)),
					if(!is.null(hw.lane)) grep(hw.lane, colnames(dd)))
					
			dd <- dd[, -drops]
		}
		#modify data frame
		x$traffic.data <- dd
		return(x)
	}

#add coordinates	
coordinatesMIDAS <- function(x, coordinates, geo.address = "Geographic.Address") { 
		#check geo.address column exist
		if(!geo.address %in% names(x))
			stop("geo.address column does not exist")
		#extract dataframe from object of class 'midas'
		dd <- as.data.frame(x)
		#split data based on geographic address
		#get a list each element represent the datafrmae 
		#for a specific geographic address
		dd[,geo.address] <- factor(dd[,geo.address], levels=unique(dd[,geo.address]))
		dd <- split(dd, dd[,geo.address])
		#match lat lon for each geographic address
		latlon <- lapply(seq(dd), function(i)
				within(dd[[i]], {
				#names(dd[i]) is the geographic address of each element in the list
				lat <- coordinates[coordinates[,1] == names(dd[i]),2]
				lon <- coordinates[coordinates[,1] == names(dd[i]),3]
				}))
		#using data.table package
		#much faster for long lists compared to do.call(rbind,l)
		dd <- rbindlist(latlon)	
		dd <- data.frame(dd)
		dd[,geo.address] <- as.character(dd[,geo.address])
		#modify data frame
		x$traffic.data <- dd
		return(x)
	}

#add details of the geographic address
#road, code, location, and direction
addDescMIDAS <- function(x, geo.address = "Geographic.Address") {
		#check geo.address column exist
		if(!geo.address %in% names(x))
			stop("geo.address column does not exist")
		#extract dataframe from object of class 'midas'
		dd <- as.data.frame(x)
		#extract information from the geographic address
		z <- data.frame(geo.address = dd[,geo.address], stringsAsFactors = FALSE)
		#splits geo.address to two based on "/"
		#road information before the "/"
		z[,2] <- sapply(strsplit(z[,1],split="/"), "[", 1)
		#code and letter infor after the "/"
		z[,3] <- sapply(strsplit(z[,1],split="/"), "[", 2)
		#check where the letter is in the geographic address after "/"
		l <- sapply(seq(z[,3]), function(i) 
				which(strsplit(z[,3][i], "")[[1]] %in% c("A", "K", "J","B", "M", "L")))
		#cut until the letter to get the code
		z[,4] <- substr(z[,3], start = 1, stop = l-1)
		#get the letter
		z[,5] <- substr(z[,3], start = l, stop = l)
		#add direction column
		z[z[,5] %in% c("A", "K", "J"),6] <- "clockwise"
		z[z[,5] %in% c("B", "M", "L"),6] <- "anticlockwise"
		#add loop detector type column
		z[z[,5] %in% c("K", "M"),7] <- "onramp"
		z[z[,5] %in% c("J", "L"),7] <- "offramp"
		z[z[,5] %in% c("A", "B"),7] <- "mainline"
		#add information to dataframe dd
		dd <- cbind(dd, stringsAsFactors = FALSE, 
					road = z[,2], code = z[,4], 
					direction = z[,6], location = z[,7])
		#modify data frame
		x$traffic.data <- dd
		return(x)
	}			
	
#cut data to the route under study	
#x is a midas object, road is motorway to be selected, 
#direction anticlockwise or clockwise
#route start and end points (geographic code) 
#...arguments to addDescMIDAS
routeMIDAS <- function(x, road, traffic.bound = c("clockwise","anticlockwise"), 
					start.pt = NULL, end.pt = NULL, 
					location.info = c("mainline", "onramp","offramp"),
					description = c("road","code","direction","location"), ...,
					drop.onramp.duplicate = NULL, drop.offramp.duplicate = NULL) {
		#check if geographic address description is available
		#if not then run addDescMIDAS function 
		if(!all(description %in% names(x))) {
			x <- addDescMIDAS(x, ...)
			message("description added to data using addDescMIDAS")
			}
		#extract dataframe from object of class 'midas'
		dd <- as.data.frame(x)
		#extract data for a specific location
		#i.e. if location.info is "mainline" only,
		#then only mainline data will be extracted
		#default to all
		dd <- dd[dd$location %in% location.info,]
		#extract data for the selected road and direction
		#more than one road or direction is fine
		dd <- dd[dd$road == road & dd$direction == traffic.bound,]
		#extract data for the selected start and end points if these are not NULL
		if(!is.null(start.pt) && !is.null(end.pt) && traffic.bound == "anticlockwise"){
			dd <- dd[dd$code <= start.pt &  dd$code >= end.pt,]
		} else if(!is.null(start.pt) && !is.null(end.pt)){
			##if clockwise or both direction (need to check)
			dd <- dd[dd$code >= start.pt &  dd$code <= end.pt,]
		}
		#sometimes multiple on/off ramp codes at the same locations
		#user need to be aware of that
		##need to check if duplicates for mainline
		if(!is.null(drop.onramp.duplicate))
			dd <- dd[!dd$code %in% drop.onramp.duplicate,]
		if(!is.null(drop.offramp.duplicate))
			dd <- dd[!dd$code %in% drop.offramp.duplicate,]
		#modify data frame
		x$traffic.data <- dd
		return(x)
	}

#cut data to the time under study		
#x is a midas object, datetime column name
#start.time and end.time are the study times in hours
cutTimeMIDAS <- function(x, start.time, end.time, datetime = "datetime") {
		if(!datetime %in% names(x))
			stop("datetime column does not exist, check datetimeMIDAS")
		#extract dataframe from object of class 'midas'
		dd <- as.data.frame(x)
		#requires lubridate package
		dd <- dd[(hour(dd[,datetime]) + minute(dd[,datetime])/60) >= start.time & 
				(hour(dd[,datetime]) + minute(dd[,datetime])/60) <= end.time,]
		#modify data frame
		x$traffic.data <- dd
		return(x)
	}

#funciton to calculate min speeds for each geographic address	
calcMinAvgSpdMIDAS <- function(x, geo.address = "Geographic.Address", 
							speed = "avg.spd", traffic.bound = NULL) {
		#check geo.address and avg speed columns exist
		if(!all(c(geo.address, speed) %in% names(x)))
			stop("check geo.address and/or speed column names")
		#extract dataframe from object of class 'midas'
		dd <- as.data.frame(x)
		#calculate minimum speed for each geographic address in 'midas' object
		min.spd <- aggregate(dd[,speed] ~ dd[,geo.address], data = dd, FUN = min)
		if(!is.null(traffic.bound) && traffic.bound == "anticlockwise")
			min.spd <- min.spd[rev(rownames(min.spd)),]
		#return minimum speed observed for each geographic address
		#and the mean of the minimum speeds over all geographic addresses
		return(list(min.avg.spd = min.spd, avg.min.avg.spd = mean(min.spd[,2]), 
					min.min.avg.spd = min(min.spd[,2])))
	}

#get number of lanes for each loop detector
laneNrMIDAS <- function(x, geo.address = "Geographic.Address", lane.nr = "Number.of.Lanes", 
						direction.info = "direction", location.info = "location", 
						traffic.bound = NULL) {
		#check geo.address, lane, and traffic bound columns exist
		if(!all(c(geo.address, lane.nr, direction.info , location.info) %in% names(x)))
			stop("check geo.address, lane.nr, direction.info , or location.info column names")
		#extract dataframe from object of class 'midas'
		dd <- as.data.frame(x)
		n <- aggregate(dd[,lane.nr] ~ dd[,geo.address] + dd[,direction.info] + dd[,location.info],
						data = dd, mean)
		names(n) <- c(geo.address, direction.info, location.info, lane.nr)
		if(!is.null(traffic.bound) && traffic.bound == "anticlockwise")
			n <- n[rev(rownames(n)),]
		return(n)
	}

#reshape from long to wide data frame
wideMacroMIDAS <- function(x, code = "code", 
						datetime = "datetime", 
						vars = c("avg.flow", "avg.flow.per.lane", "avg.spd", "avg.den", "avg.den.per.lane"),...,
						traffic.bound = NULL) {
		#check code, lane, and traffic bound columns exist
		if(!all(c(code, datetime, vars) %in% names(x)))
			stop("check code, datetime, or vars column names")
		dd <- as.data.frame(x)
		if(!is.null(traffic.bound) && traffic.bound  == "anticlockwise") {
			mm 	<- lapply(seq(vars), function(i) {
					v <- reshape(dd[,c(datetime,code,vars[i])], 
						timevar=code, idvar=datetime, v.names=vars[i], direction="wide",...)
					v <- v[,c(colnames(v)[1],rev(colnames(v)[2:ncol(v)]))]})
		} else {
			mm 	<- lapply(seq(vars), function(i) {
					v <- reshape(dd[,c(datetime,code,vars[i])], 
						timevar=code, idvar=datetime, v.names=vars[i], direction="wide",...)
					v <- v[,colnames(v)]})
		}
		return(mm)
		}
	