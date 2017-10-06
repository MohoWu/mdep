#create midas object out of the midas .csv files
#data is the original midas file
#date is the date column name
##maybe modify to add a description of each column in the traffic.data	
createMIDAS <- function (data, date = "Date", date.format = NULL) {
		#extract date from data
		if(!date %in% names(data)) stop("date column does not exist") 
		#or get(x = date, pos = data) or if date in the form of date = Date then d <- eval(expr=substitute(date), envir=data)
		#for some reason to figure out, args.createMIDAS in readMIDAS does not work when date is not Date
		#it works when ... is put in createMIDAS instead of do.call(createMIDAS, args.createMIDAS)
		d <- data[,date] 
		#original format of the midas Date column 
		if(is.null(date.format)) date.format <- "%d/%m/%y"
		#extract date(s)
		if(class(d)[1] == "POSIXct" && class(d)[2] == "POSIXt")
			d <- as.character(as.data.frame(table(as.Date(d)))$Var1)
		else
			d <- as.character(as.data.frame(table(as.Date(d, format = date.format)))$Var1)
		#add information on the date and weekday of the midas file
		collection.date <- list(d, weekdays(as.Date(d)))
		#output is a list of the data frame and the date of the traffic data
		output <- list(traffic.data = data, collection.date = collection.date)
		class(output) <- "midas"
		return(output)
	}

#... for read.csv extra.args
#args.createMIDAS
readMIDAS <- function(file.name = file.choose(), 
			col.classes.m = c("character", rep("NULL", 4), rep("factor", 2), rep("numeric", 33)), 
			na.strings.m = "-1", ..., args.createMIDAS = list()) {
			#read file
			output <- read.csv(file = file.name, colClasses = col.classes.m, na.strings = na.strings.m, ...)
			#create 'midas' object
			output <- do.call(createMIDAS, c(list(data = output), args.createMIDAS))
			return(output)
		}