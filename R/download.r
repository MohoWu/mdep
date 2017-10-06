#use the HA decoder to decode to .tcd to .csv
decodeTCDtoCSV <- function(decoder.exe, dest.file, ...) {
	system(paste(decoder.exe,  dest.file), ...)
	}

#first method to download .tcd file, unzip it, and decode it .tcd using the decoder
#asks for the username and password when run
downloadMIDAScsv <- 
	function(dest.dir, rcc, day, ...,
		protocol = "https", 
		tcd.format = ".tcd", zip.format = ".bz2", csv.format = ".csv", 
		remove.zip.file = TRUE, unzip = TRUE, 
		remove.tcd.file = TRUE, decode = FALSE, decoder.exe = NULL) {
			
			#asks for username and password 
			username <- readline("Please enter your MIDAS Username: ")  
			password <- readline("Please enter your MIDAS Password: ") 
			
			#currently these are the available regional centres
			if(length(which(!rcc %in% c(10, 20, 30, 40, 50, 60, 70, 79))) > 0)
				stop("double check RCC values!")
			
			#transform day in the file name to data
			nday <- as.POSIXct(strptime(day, format = "%d%m%y", "GMT"))	
			#extract the month
			#month requires lubridate package
			month <- month.abb[month(nday)]
			#extract the year
			year <- year(nday)
			
			#url where midas traffic data exists
			base.url  <- "@www.midas-data.org.uk/midasdata/Trafdata"		
			midas.url <- paste(protocol, "://", username, ":", password, base.url, 
							"/Co", rcc, "/", year, "/", month, "/", rcc,
							day, tcd.format, zip.format, sep="")
			dest.zip.file <- paste(dest.dir, "/", rcc, day, tcd.format, zip.format, sep = "") 
			dest.tcd.file <- paste(dest.dir, "/", rcc, day,  tcd.format, sep = "") 
			dest.csv.file <- paste(dest.dir, "/", rcc, day,  tcd.format, csv.format, sep = "") 			
				
			#curl requires Rcurl to be installed on desktop
			#curl specific for https
			if (protocol == "https") download.method <- "curl"			
			#warning if any zip file already exists
			if(length(which(file.exists(dest.zip.file))) > 0)
				warning(paste("zip file", day[which(file.exists(dest.zip.file))], "already exists\n"))
			#if multiple files are to be downloaded sapply needed for download.file
			#get the index of the files (zip, tcd, or csv) which already exist so not to download again
			f <- Reduce(union, list(which(file.exists(dest.zip.file)), 
						which(file.exists(dest.tcd.file)), which(file.exists(dest.csv.file))))
			#download file when its corresponding zip, tcd, or csv does not already exist 
			output <- sapply(seq(length(dest.zip.file))[!seq(length(dest.zip.file)) %in% f], function(i) 
							download.file(url = midas.url[i], 
								destfile = dest.zip.file[i], 
								method = download.method))
			
			#warning if any .tcd file already exists
			if(unzip == TRUE && length(which(file.exists(dest.tcd.file))) > 0)
				warning(paste("tcd file", day[which(file.exists(dest.tcd.file))], "already exists\n"))	
			#bunzip2 requires R.utils
			#if multiple files are to be downloaded sapply needed for bunzip2
			if(unzip == TRUE)
				#get the index of the files (tcd, or csv) which already exist so not to download again
				f <- union(which(file.exists(dest.tcd.file)), which(file.exists(dest.csv.file)))
				#unzip file when its corresponding tcd or csv does not already exist 
				#bunzip2 requires R.utils package
				output <- sapply(seq(length(dest.tcd.file))[!seq(length(dest.tcd.file)) %in% f], function(i)
							bunzip2(filename = dest.zip.file[i], remove = remove.zip.file))
			
			#warning if any .csv file already exists
			if(decode == TRUE && length(which(file.exists(dest.csv.file))) > 0)
				warning(paste("csv file", day[which(file.exists(dest.csv.file))], "already exists\n"))			
			#make sure path for the decoder is there
			if(decode == TRUE && is.null(decoder.exe)) {
				stop(".exe not found. Please provide full path")
			} else if(decode == TRUE && !is.null(decoder.exe)) { 
					output <- sapply(which(!file.exists(dest.csv.file)), function(i)
								decodeTCDtoCSV(decoder.exe, dest.tcd.file[i], ...))
					#remove .tcd, keep .csv
					if(remove.tcd.file == TRUE) 
						#remove existing tcd after decoding
						file.remove(dest.tcd.file[which(file.exists(dest.tcd.file))])
			} 
		invisible(output)
		}