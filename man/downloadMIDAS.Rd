\name{downloadMIDAS}
\alias{downloadMIDAScsv}
\alias{decodeTCDtoCSV}

\title{
downloads tcd from the MIDAS server and decode to csv files
}
	
\description{
Function to download .tcd.bz2 traffic data file(s) from MIDAS  
using your obtained username and password for specified regional control centre(s) (rcc) and day(s).
The function unzips and decodes .tcd file(s) 
BUT it requires the Highways England decoder (tcd to csv decoder) to be installed on your system.

MIDAS stores data in .tcd format (traffic counting data). 
\link{decodeTCDtoCSV} is mainly used in \link{downloadMIDAScsv}
to automatically decode the downloaded file to csv.
}
	
\usage{
downloadMIDAScsv(dest.dir, rcc, day, ...,
		protocol = "https", 
		tcd.format = ".tcd", zip.format = ".bz2", csv.format = ".csv", 
		remove.zip.file = TRUE, unzip = TRUE, 
		remove.tcd.file = TRUE, decode = FALSE, decoder.exe = NULL)

decodeTCDtoCSV(decoder.exe, dest.file, ...)
	}
	
\arguments{
\item{dest.dir}{
	character. directory where midas traffic data will be downloaded
	}
\item{rcc}{
	character/numeric vector. regional control centre(s). e.g. "70" or if multiple files e.g. c("70",70")
	Currently, midas network divided into 8 regions 
	(10,20,30,40,50,60,70,79)
	}
\item{day}{
	character. day(s) of the traffic data to be downloaded of format "ddmmyy" e.g. "010414" or 
	if multiple files c("010414", "020414")
	}
\item{\dots}{
	(optional) additional arguments to system() in \link{decodeTCDtoCSV}
	}
\item{protocol}{
	character. http/https. Default to "https"
	}
\item{tcd.format, zip.format, csv.format}{
	character. file extensions. Defualt to ".tcd", ".bz2", and ".csv"
	}
\item{remove.zip.file}{
	logical value. TRUE to remove the downloaded zipped file.
	FALSE to keep it in the directory. Default to TRUE
	}
\item{unzip}{
	logical value. TRUE to unzip the downloaded file.
	FALSE not to. Default to TRUE
	}
\item{remove.tcd.file}{
	logical value. TRUE to remove the unzipped tcd file after decoding it to csv.
	FALSE to keep it in the directory. Default to TRUE
	}
\item{decode}{
	logical value. TRUE to decode tcd to csv using MIDAS decoder.
	FALSE not to. Default to FALSE. If TRUE, decoderEXEFile is needed
	}
\item{decoder.exe}{
	full path of the .exe of the MIDAS TCD Decoder on your system
	(Highways England, 2016). Needed only if decode is TRUE
	}
\item{dest.file}{
	full path of the decoded unzipped .tcd file
	}	
}
	
\value{
.tcd.bz2 midas data file(s) in the directory folder if remove.zip.file is FALSE
unzipped .tcd midas data file(s) in the directory folder if remove.tcd.file is FALSE
unzipped decoded .csv midas data file(s) in the directory folder if decode = TRUE

.tcd.bz2 midas data file(s) in the directory folder if remove.zip.file is FALSE
unzipped .tcd midas data file(s) in the directory folder if remove.tcd.file is FALSE
unzipped decoded .csv midas data file(s) in the directory folder if decode = TRUE

unzipped decoded .csv midas data file in the directory folder (dest.file)
}

\note{
these require cURL to be installed on PC 
(curl-7.40.0-win64.msi - restart after installation).
Also, these require access to MIDAS system 
(username/password  required).
If using \link{decodeTCDtoCSV} or decode = TRUE \link{downloadMIDAScsv}, 
the decoder needs to be installed on your system which is a 
HATMS application: MIDAS TCD Decoder D: TCDtoCSVConsole.exe. 
Can be downloaded once access to the MIDAS system is granted from the Highways England.
}

\examples{
#download one file
\dontrun{downloadMIDAScsv(dest.dir = getwd(), 
			rcc = "70", day = "010410", decode=TRUE,
			decoder.exe = '"~/.TCDtoCSVConsole.exe"')
}}